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
    
    studiesSelected <- reactive({
        input$studies
    })
    
    studiesDataInput <- reactive({
        patient_sample_cleaned %>% filter(is.element(study_id, input$studies))
    })
    
    output$studiesSelected <- renderText(
        paste(studiesSelected(), collapse = ", ")
    )
    
    output$plot1 <- renderPlot({
        ggplot(studiesDataInput()) + 
            geom_boxplot(aes(study_id, normalised_mut_count)) + 
            scale_y_continuous(limits = c(0, 200)) +
            theme(axis.text.x = element_text(angle = 90))
    })
    
    output$table1 <- renderTable(
        studiesDataInput() %>% select(study_id, patient_id, sample_id, normalised_mut_count)
    )
    
    output$plot2 <- renderPlot({
        plot(cars, col='red')
    })
    
    output$table2 <- renderTable(
        cars
    )
    
    clinicalDataVar <- reactive({
        input$`clinical variable`
    })
    
    output$plot3 <- renderPlot({
        if (clinicalDataVar() == "tumor stage") {
            ggplot(patient_sample_cleaned) + geom_violin(aes(x = stage_at.presentation, y = normalised_mut_count, fill = stage_at.presentation))
        } else if(clinicalDataVar() == "tumor type"){
            ggplot(patient_sample_cleaned) + geom_violin(aes(x = cancer_type, y = normalised_mut_count, fill = cancer_type))
        } else {
            
        }
        
    })
    
    output$table3 <- renderTable(
        if (clinicalDataVar() == "tumor stage") {
            patient_sample_cleaned %>% select(study_id, patient_id, sample_id, stage_at.presentation, normalised_mut_count)
        } else if(clinicalDataVar() == "tumor type"){
            patient_sample_cleaned %>% select(study_id, patient_id, sample_id, cancer_type, normalised_mut_count)
        } else {
            
        }
    )

})
