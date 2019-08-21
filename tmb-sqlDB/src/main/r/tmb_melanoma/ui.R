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
                                selected = unique(patient_sample_cleaned$study_id))
             ),
             
             mainPanel(
                 tabsetPanel(
                     tabPanel("Plot", 
                              plotOutput("plot1"),
                              p("selected studies: "),
                              textOutput('studiesSelected')), 
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
                              plotOutput("plot2b")), 
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
                     tabPanel("Plot", 
                              p("TMB Distribution against Cancer Stage"),
                              plotOutput("plot3"),
                              p(),
                              p(),
                              p(),
                              p("Note: The Cancer stage described here utilizes AJCC staging system and applied to all the studies. M stage 
                                is categorized as stage IV ")),
                     tabPanel("Table", tableOutput("table3"))
                 )
             )
             )
)
)
