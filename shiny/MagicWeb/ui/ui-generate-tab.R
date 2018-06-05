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

#######
# GENERATE.R
#######

tabPanel(value = "generate",
         title = "Generate",
         sidebarLayout(
           mainPanel = mainPanel(
             conditionalPanel(
               condition = "input.generateModelsButton",
               tabsetPanel(
                 id = "generatePlots",
                 tabPanel("Summary Table",
                          dataTableOutput("modelTbl")
                 ),
                 tabPanel("Precision-recall plot",
                          plotOutput("modelPlots",
                                     height = 480,
                                     width = 700
                          )
                          
                 )
               )
             )
           ),
           sidebarPanel = sidebarPanel(
             actionButton("next_analyze", "Press to skip generate step"),
             tags$p(HTML("<br><b>Select model output directory</b>")),
             shinyDirButton("generateOutput", "Output directory", "Upload"),
             verbatimTextOutput("generateOutput", placeholder = TRUE),
             br(),
             shinyFilesButton('trainingFile', 'Upload file with processed chromatin data', 'for test, please use joined_scores_percentile_GM12878.txt', multiple = F),
             verbatimTextOutput("trainingFileText", placeholder = FALSE),
             br(),
             selectizeInput(
               "modelList",
               "Select models to train",
               choices = model_list,
               multiple=TRUE
             ) %>%
               shinyInput_label_embed(
                 icon("info") %>%
                   bs_embed_tooltip(title = "Please select at least one")
               ),
             selectizeInput(
               inputId = 'tg',
               label = 'Select training genes',
               choices = tg_names,
               selected = 'human'
             ) %>%
               shinyInput_label_embed(
                 icon("info") %>%
                   bs_embed_tooltip(title = "If you want to train model with our MAE/BAE classification, select human or mouse option")
               ),
             numericInput("trainingPercent",
                          "Fraction of data to use for training:", 80,
                          min = 0, max = 100, step = 10),
             # optional parameters
             bsCollapse(id = "collapseExample", open = "Panel",
                        bsCollapsePanel("Optional parameters",
                                        shinyFilesButton('tg2', 'Upload file with genes for training', 'If you have your own MAE/BAE classification for training, select this file here, see documentation for the file format', multiple = F),
                                        verbatimTextOutput("tg2Text", placeholder = FALSE),
                                        textInput('targetFeature', "Target Feature (the name of the column with class labels, normally 'status')",
                                                  placeholder = 'Enter name of target column',
                                                  value="status"
                                        ),
                                        textInput("positiveClass",
                                                  label = "Enter positive class (the class label being predicted, normally 'MAE')",
                                                  value="MAE"
                                        ),
                                        selectizeInput(
                                          'metric', 'Select Loss Function',
                                          choices = metric_names
                                        ),
                                        selectizeInput(
                                          'samplingMethod', 'Sampling Method',
                                          choices = sampling_method_names
                                        ),
                                        selectizeInput(
                                          'selectionRule', 'Selection Rule',
                                          choices = selection_rules
                                        ),
                                        selectizeInput(
                                          'crossValidation',
                                          "Number of cross-validations",
                                          choices=c(2:20),
                                          selected=5
                                        ),
                                        h4(HTML("External validation (optional)")),
                                        shinyFilesButton('validationSet', 'Upload an external validation set if you have one', 'If you have file for external validation, use it here. Please see documentation for the file format', multiple = F),
                                        verbatimTextOutput("validationSetText", placeholder = FALSE),
                                        style = "success")),
             actionButton("generateModelsButton", "Generate models", width = "100%")
           ))
)
