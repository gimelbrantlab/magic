# Copyright (C) 2017 Dana-Farber Cancer Institute Inc.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Questions, comments and concerns can be directed to
#   Alexander Gimelbrant: alexander_gimelbrant@dfci.harvard.edu
#   Sebastien Vigneau: Sebastien_Vigneau@dfci.harvard.edu
#   Svetlana Vinogradova: Svetlana_Vinogradova@dfci.harvard.edu
#   Henry Ward: henry.neil.ward@gmail.com
#   Sachit Saksena: sachitdsaksena@utexas.eduA

########
# PROCESS.R
########

tabPanel(value = "process",
        title = "Process",
         sidebarLayout(
           sidebarPanel = sidebarPanel(
              tags$h3(HTML("<u>Input to Processing</u>")),                 
              h4( HTML("First, set a path to where your data lives.")),
              shinyDirButton("dir", "Choose input directory", "Upload"),
              h4( HTML("Now choose your output folder")),
              shinyDirButton("outputPath", "Choose output directory", "Upload"),
              textInput(
                'fileInput',
                label = h5("Text file with names of marks (e.g., 'input.txt'):"),
                value = "input.txt"
              ),
              br(),
             selectizeInput('assembly', 'Assembly',
                               choices = c(assembly, "other"),
                               selected = "hg19"),
             conditionalPanel("input.assembly == 'other",
                              fileInput("bed",
                                        label = "If you don't use an assembly above, you can upload a .bed file of your assembly (optional)",
                                        accept = c(".bed")
                                        )
                              ),
             numericInput("promoterLength", "Select promoter length:", 0, min = 0,
                          step = 100),
             selectizeInput("enableFilters",
                           "Choose filters (gene groups to remove from analysis)",
                            choices = filtering,
                            multiple=TRUE),
             checkboxInput("noOverlap",
                           "Disable overlap of promoter and gene body?",
                           value = FALSE),
             sliderInput("dropPercent", "Choose the bottom percentile of genes to drop (to eliminate genes with low input):", 0.05,
                         min = 0, max = 0.80, step = 0.01),
             selectizeInput("cores",
                            "Number of cores to run process with:",
                             choices = c(1:12)
             ),
             actionButton("processDataButton", 
                          "Process data", 
                          width = "100%")
           ),
           mainPanel = mainPanel(
             conditionalPanel(
               condition = "input.processDataButton",
                              tabsetPanel(
                               id = "processPlots",
                               tabPanel("Processed Data Table",
                                        sidebarLayout(
                                          mainPanel(
                                                dataTableOutput("perc_table")
                                          ),
                                          mainPanel(
                                            NULL
                                          )
                                        )
                               ),
                               tabPanel("ChIP QC",
                                       sidebarLayout(
                                         mainPanel(
                                                h3("Density plots for chromatin marks, normalized values:"),
                                                plotOutput("chipQC_density",
                                                           height = 500,
                                                           width = 800,
                                                           click = "plot1_click",
                                                           brush = brushOpts(
                                                             id = "plot1_brush")
                                                ),
                                                h3("Scatterplots for chromatin marks, quantiles:"),
                                                plotOutput("chipQC",
                                                           height = 600,
                                                           width = 600,
                                                           click = "plot2_click",
                                                           brush = brushOpts(
                                                             id = "plot2_brush")
                                                )
                                         ), 
                                         mainPanel(
                                           NULL
                                         )
                                )
                               )
                               ,
                               tabPanel("Input distribution",
                                        sidebarLayout(
                                          mainPanel(
                                            h3("Density plots for input values, gene body and promoter (if selected):"),
                                            plotOutput("inputDist",
                                                       height = 600,
                                                       width = 900
                                                       )

                                            ),
                                          mainPanel(
                                            NULL
                                          )
                                          )
                                        )
                              )
             )
             
             ), # end of conditionalPanel
             fluid = TRUE
     
      )
      
)

