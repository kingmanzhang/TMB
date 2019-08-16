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
shinyServer(function(input, output) {
    
    # load data
    data <- read_excel("./Melanoma_Combined_File.xls", sheet = 1)

    output$distPlot <- renderPlot({
        
        filtered <- data %>% 
            #select(`Patient ID`,`Sample ID`, `Stage at Presentation`, `Mutation Count`) %>% 
            mutate(`Mutation Count` = as.integer(`Mutation Count`)) %>%
            distinct()
        
        x_var = input$x_var
        if (x_var == "Stage at Presentation") {
            ggplot(filtered) + 
                geom_point(aes(x = `Stage at Presentation`, y = `Mutation Count`)) +
                title("TMB ~ Stage at Presentation")
        } else if (x_var == "Oncotree Code"){
            ggplot(filtered) + 
                geom_point(aes(x = `Oncotree Code`, y = `Mutation Count`)) +
                title("TMB ~ Oncotree Code")
        } else{
            
        }
        
        

    })
    
    #output$x_var = 

})
