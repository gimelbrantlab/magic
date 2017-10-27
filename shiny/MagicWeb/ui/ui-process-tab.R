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
                               # tabPanel("Option 1: File",
                               #                 sidebarLayout(
                               #                     mainPanel(
                               #                       tags$h3(HTML("<u>Input to Processing</u>")),
                               #                       p(HTML("To use this method input a file following the format below with fields seperated by tabs
                               #                              in a text file format:<br>
                               #                              <table style='width:100%'>
                               #                              <tr>
                               #                              <th>mark name</th>
                               #                              <th>mark file</th>
                               #                              <th>control file</th>
                               #                              </tr>
                               #                              <tr>
                               #                              <td>mark1</td>
                               #                              <td>input_dir/sample_mark1.bigWig</td>
                               #                              <td>input_dir/control_mark1.bigWig</td>
                               #                              </tr>
                               #                              <tr>
                               #                              <td>mark2</td>
                               #                              <td>input_dir/sample_mark2.bigWig</td>
                               #                              <td>input_dir/sample_mark2.bigWig</td>
                               #                              </tr>
                               #                              </table>")),
                               #                       br(),
                               #                       p("The program will now automatically pick the filenames you provided when you
                               #                         move to the processing tab."),
                               #                       width = 9
                               #                       ),
                               #                   fluidRow(
                               #                     sidebarPanel(
                               #                         textInput(
                               #                           'fileInput',
                               #                           label = h5("Fill in the text field below with the name of the text file with names of marks. Ex. 'input.txt'"),
                               #                           value = NULL
                               #                         ),
                               #                       width = 9
                               #                     )
                               #                   ))),
              tags$h3(HTML("<u>Input to Processing</u>")),                 
              h4( HTML("First, set a path to where your data lives.")),
              shinyDirButton("dir", "Chose input directory", "Upload"),
              h4( HTML("Now choose your output folder")),
              shinyDirButton("outputPath", "Chose output directory", "Upload"),
              textInput(
                'fileInput',
                label = h5("Fill in the text field below with the name of the text file with names of marks. Ex. 'input.txt'"),
                value = NULL
              ),
              br(),
             selectizeInput(
               'organism', 'Model organism',
               choices = organism,
               selected = "human"
             ),
             selectizeInput('assembly', 'Assembly',
                               choices = c(assembly, "other"),
                               selected = "hg19"),
             conditionalPanel("input.assembly == 'other",
                              fileInput("bed",
                                        label = "If you don't use an assembly above, you can upload a .bed file of your assembly (optional)",
                                        accept = c(".bed")
                                        )
                              ),
             # conditionalPanel(
             #   condition = "input.organism == 'mouse'",
             #   selectizeInput('assembly', 'Assembly',
             #                  choices = mouse_refseq_reference,
             #                  selected = "mm9")
             #   ),
             # conditionalPanel(
             #   condition = "input.organism == 'human'",
             #   selectizeInput("assembly", 'Assembly',
             #                  choices = human_refseq_reference,
             #                  selected = "hg19")
             # ),
             # conditionalPanel(
             #   condition = "input.organism == 'other'",
             #   fileInput(
             #     'assembly',
             #     label = h5("Upload file"),
             #     accept = c(
             #       'text/comma-separated-values',
             #       'text/tab-separated-values',
             #       '.tsv'
             #     )
             #   )
             # ),
             numericInput("promoterLength", "Select fixed promoter length:", 5000, min = 0,
                          step = 100),
             selectizeInput("enableFilters",
                           "Choose filters (gene groups to remove from analysis)",
                            choices = filtering,
                            multiple=TRUE),
             checkboxInput("noOverlap",
                           "Disable overlap of promoter and gene body?",
                           value = FALSE),
             sliderInput("dropPercent", "Choose the bottom percentile of genes to drop (to eliminate genes with low input):", 0.01,
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
                               tabPanel("Tables",
                                        sidebarLayout(
                                          mainPanel(
                                                dataTableOutput("norm_table")
                                          ),
                                          mainPanel(
                                            NULL
                                          )
                                        )
                               ),
                               tabPanel("ChIP QC",
                                       sidebarLayout(
                                         mainPanel(
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
                                           NULL
                                         )
                                )
                               )
                              )
             )
             
             ), # end of conditionalPanel
             fluid = TRUE
     
      )
      # conditionalPanel(condition = "input.downloadProcessButton",
      #                  fluidRow(column(12,
      #                                  actionButton("next_generate", "Next"),
      #                                  tags$style(type="text/css", "#next_generate { width:10%; margin-left: 1000px;}")
      #                  )
      #                  )
      # )
      
)
