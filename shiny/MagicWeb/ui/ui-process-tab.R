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
#   Sachit Saksena: sachitdsaksena@utexas.edu

########
# PROCESS.R
########

tabPanel(value = "process",
        title = "Process",
         sidebarLayout(
           sidebarPanel = sidebarPanel(
             selectizeInput(
               'organism', 'Model organism',
               choices = organism,
               selected = "human"
             ),
             conditionalPanel(
               condition = "input.organism == 'mouse'",
               selectizeInput('assembly', 'Assembly',
                              choices = mouse_refseq_reference,
                              selected = "mm9")
               ),
             conditionalPanel(
               condition = "input.organism == 'human'",
               selectizeInput("assembly", 'Assembly',
                              choices = human_refseq_reference,
                              selected = "hg19")
             ),
             conditionalPanel(
               condition = "input.organism == 'other'",
               fileInput(
                 'assembly',
                 label = h5("Upload file"),
                 accept = c(
                   'text/comma-separated-values',
                   'text/tab-separated-values',
                   '.tsv'
                 )
               )
             ),
             selectizeInput(
               inputId = 'tg', 
               label = 'Select training genes',
               choices = tg_names,
               selected = 'none'
             ),
             numericInput("promoterLength", "Promoter Length:", 5000, min = 0,
                          step = 100),
             selectizeInput("enableFilters",
                           "Choose filters",
                            choices = filtering,
                            multiple=TRUE),
             checkboxInput("noOverlap",
                           "Disable overlap calculation?",
                           value = FALSE),
             sliderInput("dropPercent", "Drop percent:", 0.01,
                         min = 0, max = 0.80, step = 0.01),
             selectizeInput("cores",
                            "Number of cores:",
                             choices = c(1:12)
             ),
             actionButton("processDataButton", 
                          "Process data", 
                          width = "100%")
           ),
           mainPanel = mainPanel(
             conditionalPanel(
               condition = "output.dummyLoad",
                              tabsetPanel("processPlots",
                               tabPanel("Tables",
                                        sidebarLayout(
                                          mainPanel(
                                            radioButtons("normPercTable",
                                                         "Raw normalized table or Percentile ranked table",
                                                         choices = c("Normalized", "Percentile ranked")),
                                            conditionalPanel("normPercTable == 'Percentile ranked'",
                                                             dataTableOutput("perc_table")),
                                            conditionalPanel("normPercTable == 'Normalized'",
                                                             dataTableOutput("norm_table"))
                                          ),
                                          mainPanel(
                                            actionButton("reProcess",
                                                         "Redo processing with new parameters")
                                          )
                                        )
                               ),
                               tabPanel("ChIP QC",
                                       sidebarLayout(
                                         mainPanel(
                                              radioButtons("normPerc",
                                                           "Raw normalized plot or Percentile ranked plot",
                                                           choices = c("Normalized", "Percentile ranked")),
                                              conditionalPanel(
                                                condition = "input.normPerc == 'Normalized'",
                                                plotOutput("chipQCnorm",
                                                           height = 480,
                                                           width = 700
                                                           )
                                              ),
                                              conditionalPanel(
                                                condition = "input.normPerc == 'Percentile ranked'",
                                                plotOutput("chipQC",
                                                           height = 480,
                                                           width = 700,
                                                           click = "plot1_click",
                                                           brush = brushOpts(
                                                             id = "plot1_brush")
                                                )
                                              )
                                         ), 
                                         mainPanel(
                                           conditionalPanel(
                                             condition = "input.normPerc == 'Percentile ranked'",
                                             actionButton("excludeToggle", "Toggle points"),
                                             actionButton("excludeReset", "Reset"),
                                             actionButton("reProcess",
                                                          "Redo processing with new dataframe")
                                           )
                                         )
                                )
                               ),
                               tabPanel(
                                 title = "Input distribution"
                               ),
                                 tabPanel(
                                   title = "Training genes",
                                   plotOutput("trainingDist",
                                     height = 480,
                                     width = 700
                                     )
                               )
                              ) # end tabsetPanel
             ) # end of overall mainPanel 
             )# end of conditionalPanel
             
             # htmlOutput("processText"),
             # conditionalPanel(condition = "output.process_output",
             #                  downloadButton("downloadProcessButton",
             #                                 "Download processed data"))
             # )
     
      ),
      conditionalPanel(condition = "output.chipQC",
                       fluidRow(column(12,
                                       actionButton("next_generate", "Next"),
                                       tags$style(type="text/css", "#next_generate { width:10%; margin-left: 1000px;}")
                       )
                       )
      )
      
)

