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
    "Tumor Mutation Burden Viewer",
    tabPanel("Study", 
             sidebarPanel(
                 # Inputs excluded for brevity
                 selectInput("tmb_source_1", "TMB Source", 
                             c("cBioportal summary", "non-silent (Hack 1)"),
                             selected = "cBioportal summary"),
                 checkboxGroupInput("studies_1", "Studies",
                                sort(unique(patient_sample_cleaned$study_id)), 
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
                              h4("Pairwise Wilcox Test"),
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
                 textInput("gene", "Gene symbol", 
                             "BRAF"), 
                 textInput("position", "Amino acid position", 
                           ""),
                 textInput('other_gene', 'Simutaneously Mutated Genes', ''),
                 checkboxGroupInput("studies_2", "Studies",
                                    sort(unique(patient_sample_cleaned$study_id)), 
                                    selected = unique(patient_sample_cleaned$study_id))
             ),
             
             mainPanel(
                 tabsetPanel(
                     tabPanel("Plot", 
                              plotOutput("plot2"), 
                              plotOutput("plot2b"),
                              h4("Pairwise Wilcox Test"),
                              uiOutput('p_matrix_2b'),
                              plotOutput('plot2c'),
                              h4("Pairwise Wilcox Test"),
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
                     
                 )
             )
             ),
    tabPanel("Clinical", 
             sidebarPanel(
                 selectInput("tmb_source_3", "TMB Source", 
                             c("cBioportal summary", "non-silent (Hack 1)"),
                             selected = "cBioportal summary"),
                 selectInput("clinical variable", "Clinical variable", 
                             c("tumor stage", "tumor type"),
                             selected = "tumor stage"),
                 checkboxGroupInput("studies_3", "Studies",
                                    sort(unique(patient_sample_cleaned$study_id)), 
                                    selected = unique(patient_sample_cleaned$study_id))
             ),
             
             mainPanel(
                 tabsetPanel(
                     tabPanel("Plot", plotOutput("plot3"), 
                              br(),
                              br(),
                              h4("Pairwise Wilcox Test"),
                              uiOutput("p_matrix_3")), 
                     tabPanel("Summary", DT::dataTableOutput("summary3")),
                     tabPanel("Table", DT::dataTableOutput("table3"))
                 )
             )
             ),
    tabPanel("Help", 
             sidebarPanel(
               h2("content")
             ),
             
             mainPanel(
               h2("What is this app about"),
               p("This is an R Shiny app to visualize tumor mutation burden (TMB) in skin cancer. The data source is 13 datasets deposited on cBioportal. Users of this app can explore TMB distribution across studies, under different genetic backgrounds or with different clinical variables."),
               br(),
               h4("How TMB is calculated"),
               p("TMB is defined as the number of mutations per coding region. However, its calculation lacks standardization in current studies. This is reflected in both the way it is assayed and the way it is calculated."),
               p("Different assays are used to measure TMB. The most accurate way is whole exome sequencing. Due to the cost and complexity, many studies also use targeted sequencing of customized or commercial cancer panels. In any case, we identified the exact length of the genomic length that is covered by the corresponding assay and use it to normalize the mutation counts."),
               p("In addition, we also harmonized how to count mutations. Across studies and assays, sometimes the silent mutations are excluded while sometimes they are included. We included both ways: if \"cBioportal Summary\" is chosen as TMB Source, the score will include silent mutations; if \"non-silent(Hack 1)\" is chosen, the score will exclude silent mutations."),
               br(),
               h3("Navigation tab: Study"),
               br(),
               h4("TMB Source"),
               p("Choose \"cBioportal summary\" to include silent mutations; choose \"non-silent (Hack 1)\" to exclude silent mutations"),
               br(),
               h4("Studies"),
               p("Use the checkboxes to include or exclude certain studies for analysis. The study ID is taken from the cBioportal dataset id."),
               br(),
               h3("Navigation tab: Gene"),
               br(),
               h4("TMB Source and Studies"),
               p("Use them in the same way as under the Study tab. Refer above for details"),
               br(),
               h4("Gene symbol"),
               p("Type in the Hugo Symbol to explore TMB distribution when the gene of interested is mutated. If no mutation is detected in any of the studies, only wild type will be shown."),
               br(),
               h4("Amino acid position"),
               p("Type in the amino acid position, such as 600, to look at TMB distribution for mutations at the specified position of a gene. WT_aa means wild type at the specified position and does NOT mean no mutations at other positions of the amino acid."),
               br(),
               h4("Simutaneously Mutated Genes"),
               p("Type in other genes, separated by space or comma, to look at TMB distribution under simultaneous mutations of those genes."),
               br(),
               h3("Navigation tab: Clinical"),
               
               br(),
               h4("TMB Source and Studies"),
               p("Use them in the same way as under the Study tab. Refer above for details"),
               br(),
               h4("Clinical variable"),
               p("Use the dropdown menu to choose which clinical variable to look at"),
               h2("Authors"),
               p("Aaron Zhang, Jackson Laboratory"),
               p("Vandhana Chezhiyan, Northeastern University"),
               p("Ting (Helen) He, Johns Hopkins University"),
               p("Dongyang Yi, Northeastern University"),
               p("For all questions, send to Aaron Zhang aaron.zhang@jax.org"),
               h2("License"),
               p("Copyright (c) 2019 Xingmin Aaron Zhang"),
               p("Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to deal
                 in the Software without restriction, including without limitation the rights
                 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
                 copies of the Software, and to permit persons to whom the Software is
                 furnished to do so, subject to the following conditions:"),
               p("The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software."),
               p("THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
                 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
                 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
                 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
                 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
                 SOFTWARE.")
             )
    )
)
)
