#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(tidyverse)
library(RSQLite)
source("script.R")

# shared data across sessions
db_url <- "tmb.sqlite"
dbcon <- dbConnect(RSQLite::SQLite(), db_url)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    ##############
    #control Nav 1
    ##############
    studiesSelected <- reactive({
        input$studies_1
    })
    
    tmbSourceInput_1 <- reactive({
        switch (input$tmb_source_1,
                "cBioportal summary" = patient_sample_cleaned  %>% mutate(tmb = normalised_mut_count), 
                "non-silent (Hack 1)" = patient_sample_cleaned %>% mutate(tmb = normalised_mut_count_non_silent)
        )
    })
    
    studiesDataInput_1 <- reactive({
        tmbSourceInput_1() %>% filter(is.element(study_id, input$studies_1))
    })
    
    output$studiesSelected <- renderText(
        paste(studiesSelected(), collapse = ", ")
    )
    
    output$plot1 <- renderPlot({
        ggplot(studiesDataInput_1()) + 
            geom_boxplot(aes(study_id, tmb, fill = assay)) + 
            scale_y_continuous(limits = c(0, 200)) + ylab("tumor mutation burden") +
            theme(axis.text.x = element_text(angle = 90))
    })
    
    output$table1 <- DT::renderDataTable(
        studiesDataInput_1() %>% select(study_id, patient_id, sample_id, tmb)
    )
    
    output$summary1 <- DT::renderDataTable(
        studiesDataInput_1() %>% 
            select(study_id, patient_id, sample_id, tmb) %>% 
            group_by(study_id) %>%
            summarise(sample_size = n(), 
                      mean = round(mean(tmb, na.rm = T), 1), 
                      `standard error` = round(sd(tmb, na.rm = T), 1), 
                      median = round(median(tmb, na.rm = T),1))
    )
    
    output$p_matrix_1 <- renderTable({
        p_test_data <- studiesDataInput_1() %>% select(study_id, tmb)
        l = split(p_test_data$tmb, p_test_data$study_id)
        m <- p_value_matrix(l)
        rownames(m) = 1:length(l)
        colnames(m) = 1:length(l)
        m
    }, rownames = TRUE
    )
    
    ##############
    #control Nav 2
    ##############
    studiesSelected2 <- reactive({
        input$studies_2
    })
    
    tmbSourceInput_2 <- reactive({
        switch (input$tmb_source_2,
                "cBioportal summary" = patient_sample_cleaned  %>% mutate(tmb = normalised_mut_count), 
                "non-silent (Hack 1)" = patient_sample_cleaned %>% mutate(tmb = normalised_mut_count_non_silent)
        )
    })
    
    studiesDataInput_2 <- reactive({
        tmbSourceInput_2() %>% filter(is.element(study_id, input$studies_2))
    })
    
    geneSymbolInput <- reactive({input$gene})
    
    geneQueryData <- reactive({
        if (length(studiesSelected2()) == 0){
            target_samples <- query_samples_with_mutant_gene(geneSymbolInput(), dbcon)
        }
        if (length(studiesSelected2()) > 0){
            target_samples <- query_samples_with_mutant_gene_within_studies(geneSymbolInput(), dbcon, studiesSelected2())
        }
        
        data_for_plot <- studiesDataInput_2() %>% 
            select(study_id, patient_id, sample_id, normalised_mut_count, normalised_mut_count_non_silent, tmb) %>% 
            left_join(
                target_samples %>% 
                    rename(study_id = Study_Id, sample_id = Tumor_Sample_Barcode) %>%
                    mutate(mutant = 'MT'), 
                by = c('study_id', 'sample_id')
            ) %>% 
            mutate(HGVSp_Short = if_else(is.na(mutant), 'WT', mutant))
    })
    
    genePositionInput <- reactive({
        as.integer(input$position)
    })
    
    genePositionQueryData <- reactive({
        if (!is.na(genePositionInput())){
            target_samples <- query_samples_with_mutant_gene_at_position_within_studies(geneSymbolInput(), genePositionInput(), dbcon, studiesSelected2())
            data_for_plot <- studiesDataInput_2() %>% 
                select(study_id, patient_id, sample_id, normalised_mut_count, normalised_mut_count_non_silent, tmb) %>% 
                left_join(
                    target_samples %>% 
                        rename(study_id = Study_Id, sample_id = Tumor_Sample_Barcode) %>%
                        mutate(mutant = 'YES'),
                    by = c('study_id', 'sample_id')
                ) %>% 
                mutate(HGVSp_Short = if_else(is.na(mutant), 'WT_aa', HGVSp_Short))
        }
    })
    
    other_genesInput <- reactive({
        strsplit(input$other_gene, ',[ ]?| ')[[1]]
    })
    
    multiGeneQuery <- reactive({
        if (length(other_genesInput())!= 0){
            genes = other_genesInput()
            # query result
            target_samples <- query_samples_with_multiple_genes_within_studies(c(geneSymbolInput(), other_genesInput()), studiesSelected2(), dbcon)
            data_for_plot <- studiesDataInput_2() %>% 
                select(study_id, patient_id, sample_id, normalised_mut_count, normalised_mut_count_non_silent, tmb) %>% 
                left_join(
                    target_samples %>% 
                        rename(study_id = Study_Id, sample_id = Tumor_Sample_Barcode),
                    by = c('study_id', 'sample_id')
                ) %>% 
                mutate(multiMutationStatus = if_else(is.na(multiMutationStatus), 'WT', multiMutationStatus))
        }
    })
    
    output$plot2 <- renderPlot({
        data <- geneQueryData()
        if (length(unique(data$HGVSp_Short)) == 2){
            l = split(data$tmb, data$HGVSp_Short)
            p = formatC(wilcox.test(l[[1]], l[[2]])$p.value, format = "e", digits = 2)
            p_label = paste("wilcox.test p value:", p, sep = '\n')
        }else {
            p_label = ''
        }
        
        # set upper limit to 0.9 percentile
        y_upper_limit <- quantile(data$tmb, 0.9, na.rm = TRUE)
        ggplot(data) + 
            geom_violin(aes(x = HGVSp_Short, y = tmb, fill = HGVSp_Short), draw_quantiles = 0.5) + 
            annotate("text", label=p_label, x = 0.7, y = y_upper_limit * 0.9) +
            scale_x_discrete(breaks = c("WT", 'MT'), labels = c("Wild Type", "Mutant")) + 
            scale_y_continuous(limits = c(0, y_upper_limit)) +
            xlab(paste(geneSymbolInput(), 'status')) + ylab("tumor mutation burden") + 
            ggtitle(paste("Tumor mutation burden ~ mutation status of", geneSymbolInput())) +
            theme(legend.position = 'na', 
                  axis.text = element_text(size = 14), 
                  axis.title = element_text(size = 14), 
                  plot.title = element_text(size = 18))
    }, width = 500, height = 300)
    
    
    output$plot2b <- renderPlot({
        if (!is.na(genePositionInput())){
            # set upper limit to 0.9 percentile
            y_upper_limit <- quantile(genePositionQueryData()$tmb, 0.9, na.rm = TRUE)
            ggplot(genePositionQueryData()) + 
                geom_violin(aes(x = HGVSp_Short, y = tmb, fill = HGVSp_Short), draw_quantiles = 0.5) +
                scale_y_continuous(limits = c(0, y_upper_limit)) +
                xlab("amino acid changes at the specified position") +
                ylab("tumor mutation burden") + 
                ggtitle(sprintf("%s mutation status at position %d", geneSymbolInput(), genePositionInput())) +
                theme(legend.position = "na",
                      axis.text = element_text(size = 14), 
                      axis.title = element_text(size = 14), 
                      plot.title = element_text(size = 18))
        }
    }, width = 600, height = 300)
    
    output$plot2c <- renderPlot({
        if (length(other_genesInput())!=0){
            y_upper_limit <- quantile(multiGeneQuery()$tmb, 0.9, na.rm = TRUE)
            ggplot(multiGeneQuery()) + 
                geom_violin(aes(x = multiMutationStatus, y = tmb, fill = multiMutationStatus), draw_quantiles = 0.5) +
                scale_y_continuous(limits = c(0, y_upper_limit)) + 
                ggtitle(sprintf("Tumor mutation burden ~ %s mutation at p.%d", geneSymbolInput(), genePositionInput())) +
                theme(legend.position = "na",
                      axis.text = element_text(size = 14), 
                      axis.title = element_text(size = 14), 
                      plot.title = element_text(size = 18))
        }
    }, width = 600, height = 300)
    
    
    output$p_matrix_2b <- renderTable({
        p_test_data <- genePositionQueryData() 
        if (!is.null(data)) {
            p_test_data <- genePositionQueryData() %>% select(HGVSp_Short, tmb)
            l = split(p_test_data$tmb, p_test_data$HGVSp_Short)
            m <- p_value_matrix(l)
            rownames(m) = 1:length(l)
            colnames(m) = 1:length(l)
            m
        }
    }, rownames = TRUE
    )
    
    output$p_matrix_2c <- renderTable({
        p_test_data <- multiGeneQuery() 
        # if (!is.null(data)) {
        #     p_test_data <- other_genesInput() %>% select(multiMutationStatus, tmb)
        #     l = split(p_test_data$tmb, p_test_data$multiMutationStatus)
        #     m <- p_value_matrix(l)
        #     rownames(m) = 1:length(l)
        #     colnames(m) = 1:length(l)
        #     m
        # }
        matrix(rnorm(25), nrow = 5)
    }, rownames = TRUE
    )
    
    
    output$table2 <- DT::renderDataTable(
        geneQueryData() %>% select(study_id, patient_id, sample_id, HGVSp_Short, tmb)
    )
    
    output$table2b <- DT::renderDataTable({
        data <- genePositionQueryData() 
        if (!is.null(data)) {
            data %>% select(study_id, patient_id, sample_id, HGVSp_Short, tmb)
        } 
    }
    )
    
    output$table2c <- DT::renderDataTable({
        data <- multiGeneQuery() 
        if (!is.null(data)) {
            data %>% select(study_id, patient_id, sample_id, multiMutationStatus, tmb)
        }    
        }
    )

    output$summary2 <- DT::renderDataTable(
        geneQueryData() %>%
            select(study_id, patient_id, sample_id, HGVSp_Short, tmb) %>%
            group_by(HGVSp_Short) %>%
            summarise(sample_size = n(),
                      mean = round(mean(tmb, na.rm = T), 1),
                      `standard error` = round(sd(tmb, na.rm = T), 1),
                      median = round(median(tmb, na.rm = T),1))
    )
    
    output$summary2b <- DT::renderDataTable({
        data <- genePositionQueryData() 
        if (!is.null(data)) {
            data %>% select(study_id, patient_id, sample_id, HGVSp_Short, tmb) %>%
                group_by(HGVSp_Short) %>%
                summarise(sample_size = n(),
                          mean = round(mean(tmb, na.rm = T), 1),
                          `standard error` = round(sd(tmb, na.rm = T), 1),
                          median = round(median(tmb, na.rm = T),1))
        } 
    }
    )
    
    output$summary2c <- DT::renderDataTable({
        data <- multiGeneQuery()
        if (!is.null(data)){
            data %>%
                select(study_id, patient_id, sample_id, multiMutationStatus, tmb) %>%
                group_by(multiMutationStatus) %>%
                summarise(sample_size = n(),
                          mean = round(mean(tmb, na.rm = T), 1),
                          `standard error` = round(sd(tmb, na.rm = T), 1),
                          median = round(median(tmb, na.rm = T),1))
            }
        }
        
    )

  
    ##############
    #control Nav 3
    ##############
    tmbSourceInput_3 <- reactive({
        switch (input$tmb_source_3,
                "cBioportal summary" = patient_sample_cleaned  %>% mutate(tmb = normalised_mut_count), 
                "non-silent (Hack 1)" = patient_sample_cleaned %>% mutate(tmb = normalised_mut_count_non_silent)
        )
    })
    
    studiesDataInput_3 <- reactive({
        tmbSourceInput_3() %>% filter(is.element(study_id, input$studies_3))
    })
    
    clinicalDataVar <- reactive({
        switch (input$`clinical variable`,
                "tumor stage" = "stage_at.presentation", 
                "tumor type" = "cancer_type"
        )
    })
    
    clinicalDataInput <- reactive({
        studiesDataInput_3() %>% select(study_id, patient_id, sample_id, clinicalDataVar() , tmb)
    })
    
    output$plot3 <- renderPlot({
        ggplot(clinicalDataInput()) + 
            geom_violin(aes_string(x = clinicalDataVar(), y = 'tmb', fill = clinicalDataVar()), draw_quantiles=0.5) + 
            scale_y_continuous(limits = c(0,200)) +
            ylab("tumor mutation burden") + 
            theme(legend.position = "na")
    })
    
    output$table3 <- DT::renderDataTable(
        clinicalDataInput()
    )
    
    output$p_matrix_3 <- renderTable({
        p_test_data <- clinicalDataInput() %>% select(clinicalDataVar(), tmb)
        l = split(p_test_data$tmb, p_test_data[,clinicalDataVar()])
        m <- p_value_matrix(l)
        names <- as.character(names(l))
        rownames(m) = names
        colnames(m) = names
        m
    }, rownames = TRUE
    )

})
