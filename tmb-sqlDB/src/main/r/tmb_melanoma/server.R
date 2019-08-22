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
    tmbrange <- reactive({
      input$slider1
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
    
    output$summary_line <- renderText(
      paste("TMB variation across different studies on Melanoma from the cbioportal")
    )
    
    output$sample_size <- renderText(
      paste("The sample sizes for the different studies are as in []:
            bcc_unige_2016[293], cscc_dfarber_2015[29],cscc_hgsc_bcm_2014[39],desm_broad_2015[20],mel_tsam_liang_2017[38],skcm_broad[121],
            skcm_broad_brafresist_2012[78],skcm_broad_dfarber[26],skcm_tcga[479], skcm_tcga_pan_can_atlas_2018[448],
            skcm_ucla_2016[39],skcm_vanderbilt_mskcc_2015[66],skcm_yale[147]"
            )
    )
    
    output$matrix <- renderTable({
      
      M <- matrix(rep(1,9),nrow=3)
      
      rownames(M)<- c("a","b","c")
      colnames(M) <- c(1,2,3)
      
      M
      }, rownames = TRUE)
    
    output$plot1 <- renderPlot({
        ggplot(studiesDataInput_1()) + 
            geom_boxplot(aes(study_id, tmb)) + coord_cartesian(ylim = tmbrange())+
        ggtitle("Different studies vs TMB")+
            theme(axis.text.x = element_text(angle = 90))
    })
    
    output$table1 <- renderTable(
        studiesDataInput_1() %>% select(study_id, patient_id, sample_id, tmb)
    )
    
    
    ##############
    #control Nav 2
    ##############
    
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
        tmb_vs_single_gene(geneSymbolInput(), dbcon, studiesDataInput_2())
    })
    
    genePositionInput <- reactive({
        as.integer(input$position)
    })
    
    genePositionQueryData <- reactive({
        if (!is.na(genePositionInput())){
            tmb_vs_single_gene_at_position(geneSymbolInput(), genePositionInput(), dbcon, studiesDataInput_2())
        }
    })
    
    output$summary_line2 <- renderText(
      paste("TMB variation across the gene and aminoacid position of choice")
    )
    
    output$plot2 <- renderPlot({
        ggplot(geneQueryData()) + 
            geom_violin(aes(x = HGVSp_Short, y = normalised_mut_count, fill = HGVSp_Short )) +
            xlab(paste(geneSymbolInput(), 'status'))
      
    })
    
    output$table2 <- renderTable(
        geneQueryData()
    )
    
    output$plot2b <- renderPlot({
        if (!is.na(genePositionInput())){
            ggplot(genePositionQueryData()) + 
                geom_violin(aes(x = HGVSp_Short, y = normalised_mut_count, fill = HGVSp_Short )) +
                xlab(sprintf("%s mutation status at position %d", geneSymbolInput(), genePositionInput()))
        }
    })
    
    
    
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
        ggplot(clinicalDataInput()) + geom_violin(aes_string(x = clinicalDataVar(), y = 'tmb'))+
        ggtitle("Stages of cancer vs TMB")
    })
    
    
    
    output$table3 <- renderTable(
        clinicalDataInput()
    )

})
