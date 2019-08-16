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
shinyUI(fluidPage(

    # Application title
    titlePanel("Tumor Mutation Burden"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("x_var",
                        "x axis variable:",
                        c("Stage at Presentation", "Oncotree Code"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h3("TMB plot"),
            p("choose x variable from the left"),
            plotOutput("distPlot")
        )
    )
))
