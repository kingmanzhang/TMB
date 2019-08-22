#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
shinyUI(navbarPage(
    "Tumor Mutation Burden Distribution In Melanoma",
    tabPanel("Study", 
             sidebarPanel(
                 # Inputs excluded for brevity
                 selectInput("tmb_source_1", "TMB Source", 
                             c("cBioportal summary", "non-silent (Hack 1)"),
                             selected = "cBioportal summary"),
                 checkboxGroupInput("studies_1", "Studies",
                                unique(patient_sample_cleaned$study_id), 
                                selected = unique(patient_sample_cleaned$study_id)),
                 sliderInput("slider1", label = "Choose the TMB range", min = 0, max = 200, value = c(0,100))
             ),
             
             mainPanel(
                 tabsetPanel(
                     tabPanel("Plot", 
                              plotOutput("plot1"),
                              textOutput('summary_line'),
                              textOutput("sample_size"),
                              uiOutput("matrix")),
                     tabPanel("Table", tableOutput("table1"))
                 )
             )
             ),
    tabPanel("Gene", 
             sidebarPanel(
                 # Inputs excluded for brevity
                 selectInput("tmb_source_2", "TMB Source", 
                             c("cBioportal summary", "non-silent (Hack 1)"),
                             selected = "cBioportal summary"),
                 textInput("gene", "gene", 
                             "BRAF"), 
                 textInput("position", "Amino acid position", 
                           ""),
                 checkboxGroupInput("studies_2", "Studies",
                                    unique(patient_sample_cleaned$study_id), 
                                    selected = unique(patient_sample_cleaned$study_id))
             ),
             
             mainPanel(
                 tabsetPanel(
                     tabPanel("Plot", 
                              plotOutput("plot2"), 
                              p(),
                              p(),
                              textOutput("summary_line2"),
                              plotOutput("plot2b")
                              ),
                     tabPanel("Table", tableOutput("table2"))
                 )
             )
             ),
    tabPanel("Clinical", 
             sidebarPanel(
                 selectInput("tmb_source_3", "TMB Source", 
                             c("cBioportal summary", "non-silent (Hack 1)"),
                             selected = "cBioportal summary"),
                 selectInput("clinical variable", "cli_var", 
                             c("tumor stage", "tumor type"),
                             selected = "tumor stage"),
                 checkboxGroupInput("studies_3", "Studies",
                                    unique(patient_sample_cleaned$study_id), 
                                    selected = unique(patient_sample_cleaned$study_id))
             ),
             
             mainPanel(
                 tabsetPanel(
                     tabPanel("Plot", plotOutput("plot3")),
                     tabPanel("Table", tableOutput("table3"))
                 )
             )
             )
)
)
