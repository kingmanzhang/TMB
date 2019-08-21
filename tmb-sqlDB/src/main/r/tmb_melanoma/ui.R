#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)


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
                              p('\n\n'),
                              #p("selected studies: "),
                              #textOutput('studiesSelected'), 
                              p('\n\n'),
                              br(),
                              br(),
                              h4("Pairwise Wilcox Test (p value)"),
                              uiOutput("p_matrix_1")
                              ), 
                     tabPanel("Summary", h4('Summary Statistics for Selected Studies'),
                              DT::dataTableOutput('summary1')),
                     tabPanel("Table", DT::dataTableOutput("table1"))
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
                 textInput('other_gene', 'Simutaneously Mutated Genes', '(maximum 2)'),
                 checkboxGroupInput("studies_2", "Studies",
                                    unique(patient_sample_cleaned$study_id), 
                                    selected = unique(patient_sample_cleaned$study_id))
             ),
             
             mainPanel(
                 tabsetPanel(
                     tabPanel("Plot", 
                              plotOutput("plot2"), 
                              br(),
                              br(),
                              plotOutput("plot2b"), 
                              br(),
                              br(),
                              plotOutput('plot2c')), 
                     tabPanel("Table", DT::dataTableOutput("table2"))
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
                     tabPanel("Plot", plotOutput("plot3"), 
                              br(),
                              br(),
                              h4("Pairwise Wilcox Test (p value)"),
                              uiOutput("p_matrix_3")), 
                     tabPanel("Table", DT::dataTableOutput("table3"))
                 )
             )
             )
)
)
