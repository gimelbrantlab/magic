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

##########
# ANALYZE TAB
##########

tabPanel(value = "analyze",
         title = "Analyze",
         sidebarLayout(
           sidebarPanel(
             tags$p(HTML("<b>Select folder with models</b>")),
             shinyDirButton("modelFolder", "Model directory", "Upload"),
             #models_folder_def <- file.path(current_folder, "/models")
             verbatimTextOutput("modelFolder"), #placeholder = models_folder_def),
             checkboxInput("bestMod", "Limit the analysis to the best model only", TRUE),
             tags$p(HTML("<b>Select output directory</b>")),
             shinyDirButton("analyzeOutput", "Output directory", "Upload"),
             verbatimTextOutput("analyzeOutput", placeholder = TRUE),
             br(),
             shinyFilesButton('analysisFile', 'Upload file with processed chromatin data', 'In most cases, it is joined_scores_percentile.txt in your output folder', multiple = F),
             verbatimTextOutput("analysisFileText", placeholder = FALSE),
             textInput("positiveClass", "Positive class:",
                       value = "MAE"),
             # Filtering part
             tags$p(HTML("<b>Filtering</b>")),
             checkboxInput("lengthFilt", "Filter by length", TRUE),
             conditionalPanel(
               condition = "input.lengthFilt",
               numericInput("lengthFilter",
                            "Set length filter",
                            value=2500) %>%
                 shinyInput_label_embed(
                   icon("info") %>%
                     bs_embed_tooltip(title = "Set length lower threshold here, if you want to filter out shorter genes")
                 ),
               selectInput(
                 "filterOrg", "Length",
                 c("human",
                   "mouse"), selected='human')
               ),
             checkboxInput("exprFilt", "Filter by expression", TRUE),
             selectInput(
               "filterFile", "Expression file",
               c("human (expression in GM12878)",
                 "mouse",
                 "custom"),selected='human'),
             conditionalPanel(
               condition = "input.filterFile == 'custom' ",
               shinyFilesButton('expressionData', 'Upload file with genes expression and lengths', 'Please see readme for file format', multiple = F),
               verbatimTextOutput("expressionDataText", placeholder = FALSE)),
             actionButton("analyzeDataButton", "Analyze data", width = "100%")
           ), mainPanel(
             conditionalPanel(
               condition = "input.analyzeDataButton",
               tabsetPanel(id = "outputPlot",
                           tabPanel("Table",
                                    dataTableOutput("predTbl")
                           ),
                           tabPanel("Plots",
                                    selectInput(
                                      "modelToPlot", "Select model",
                                      model_list, selected='human'),
                                    plotOutput("analyzePlots",
                                               height = 480,
                                               width = 700
                                    )
                                    
                           )
               )
             )
           )
         )
)

