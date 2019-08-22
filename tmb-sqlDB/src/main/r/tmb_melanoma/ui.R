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
<<<<<<< HEAD
                                unique(patient_sample_cleaned$study_id), 
                                selected = unique(patient_sample_cleaned$study_id)),
                 sliderInput("slider1", label = "Choose the TMB range", min = 0, max = 200, value = c(0,100))
=======
                                sort(unique(patient_sample_cleaned$study_id)), 
                                selected = unique(patient_sample_cleaned$study_id))
>>>>>>> develop
             ),
             
             mainPanel(
                 tabsetPanel(
                     tabPanel("Plot", 
                              plotOutput("plot1"),
<<<<<<< HEAD
                              textOutput('summary_line'),
                              textOutput("sample_size"),
                              uiOutput("matrix")),
                     tabPanel("Table", tableOutput("table1"))
=======
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
>>>>>>> develop
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
                 textInput('other_gene', 'Simutaneously Mutated Genes', ''),
                 checkboxGroupInput("studies_2", "Studies",
                                    unique(patient_sample_cleaned$study_id), 
                                    selected = unique(patient_sample_cleaned$study_id))
             ),
             
             mainPanel(
                 tabsetPanel(
                     tabPanel("Plot", 
                              plotOutput("plot2"), 
<<<<<<< HEAD
                              p(),
                              p(),
                              textOutput("summary_line2"),
                              plotOutput("plot2b")
                              ),
                     tabPanel("Table", tableOutput("table2"))
=======
                              plotOutput("plot2b"), 
                              uiOutput('p_matrix_2b'),
                              plotOutput('plot2c'),
                              uiOutput('p_matrix_2c')), 
                     tabPanel("Summary", 
                              DT::dataTableOutput("summary2"), 
                              DT::dataTableOutput("summary2b"), 
                              DT::dataTableOutput("summary2c")
                              ),
                     tabPanel("Table", 
                              DT::dataTableOutput("table2"), 
                              DT::dataTableOutput("table2b"), 
                              DT::dataTableOutput("table2c"))
                     
>>>>>>> develop
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
<<<<<<< HEAD
                     tabPanel("Plot", plotOutput("plot3")),
                     tabPanel("Table", tableOutput("table3"))
=======
                     tabPanel("Plot", plotOutput("plot3"), 
                              br(),
                              br(),
                              h4("Pairwise Wilcox Test (p value)"),
                              uiOutput("p_matrix_3")), 
                     tabPanel("Summary", DT::dataTableOutput("summary3")),
                     tabPanel("Table", DT::dataTableOutput("table3"))
>>>>>>> develop
                 )
             )
             )
)
)
