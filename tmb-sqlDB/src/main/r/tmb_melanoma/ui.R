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
                 selectInput("tmb_source", "TMB Source", 
                             c("cBioportal summary", "computed from raw"),
                             selected = "cBioportal summary"),
                 checkboxGroupInput("studies", "studies",
                                c("Study 1"= "S1", "Study 2" = "S2", "Study 3" = "S3"),
                                selected = c("S1", "S2", "S3"))
             ),
             
             mainPanel(
                 tabsetPanel(
                     tabPanel("Plot", plotOutput("plot1")), 
                     tabPanel("Table", tableOutput("table1")),
                     p("selected studies"),
                     textOutput("studies_selected")
                 )
             )
             ),
    tabPanel("Gene", 
             sidebarPanel(
                 # Inputs excluded for brevity
                 selectInput("tmb_source", "TMB Source", 
                             c("cBioportal summary", "computed from raw"),
                             selected = "cBioportal summary"),
                 textInput("gene", "gene", 
                             "BRAF")
             ),
             
             mainPanel(
                 tabsetPanel(
                     tabPanel("Plot", plotOutput("plot2")), 
                     tabPanel("Table", tableOutput("table2"))
                 )
             )
             ),
    tabPanel("Clinical", 
             sidebarPanel(
                 # Inputs excluded for brevity
                 selectInput("tmb_source", "TMB Source", 
                             c("cBioportal summary", "computed from raw"),
                             selected = "cBioportal summary"),
                 selectInput("clinical variable", "cli_var", 
                             c("tumor stage", "tumor type"),
                             selected = "tumor stage")
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
