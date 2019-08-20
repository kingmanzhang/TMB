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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    output$studies_selected <- renderText({
        studies <- paste(input$studies, collapse = ", ")
        paste("You chose", studies)
    })
    
    output$plot1 <- renderPlot({
        plot(cars)
    })
    
    output$table1 <- renderTable(
        cars
    )
    
    output$plot2 <- renderPlot({
        plot(cars, col='red')
    })
    
    output$table2 <- renderTable(
        cars
    )
    
    output$plot3 <- renderPlot({
        plot(cars, col='blue')
    })
    
    output$table3 <- renderTable(
        cars
    )

})
