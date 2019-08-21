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

# shared data across sessions
patient_sample_cleaned <- read.csv("patient_sample_cleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ',')

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
            geom_boxplot(aes(study_id, tmb)) + 
            scale_y_continuous(limits = c(0, 200)) +
            theme(axis.text.x = element_text(angle = 90))
    })
    
    output$table1 <- renderTable(
        studiesDataInput_1() %>% select(study_id, patient_id, sample_id, tmb)
    )
    
    
    ##############
    #control Nav 2
    ##############
    
    output$plot2 <- renderPlot({
        plot(cars, col='red')
    })
    
    output$table2 <- renderTable(
        cars
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
        ggplot(clinicalDataInput()) + geom_violin(aes_string(x = clinicalDataVar(), y = 'tmb'))
    })
    
    output$table3 <- renderTable(
        clinicalDataInput()
    )

})
