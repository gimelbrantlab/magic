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
         title = "Model",
         sidebarLayout(
           # mainPanel = mainPanel(NULL),
           mainPanel = mainPanel(
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
           ),
           sidebarPanel = sidebarPanel(
             tabsetPanel(
               id = "options",
               tabPanel("New", 
                        tags$p(HTML("<br><b>Select model output directory</b>")),
                        shinyDirButton("generateOutput", "Output directory", "Upload"),
                        verbatimTextOutput("generateOutput", placeholder = TRUE), 
                        #verbatimTextOutput("outputPath", placeholder = TRUE),
                        br(),
                        br(),
                        fileInput('trainingFile', 'Upload file with processed chromatin data',
                                  accept = acceptable_file_types) %>% 
                          shinyInput_label_embed(
                            icon("info") %>%
                              bs_embed_tooltip(title = "In most cases, it is joined_scores_percentile.txt in your output folder")
                          ),
                        selectizeInput(
                          inputId = 'tg', 
                          label = 'Select training genes',
                          choices = tg_names,
                          selected = 'none'
                        ) %>% 
                          shinyInput_label_embed(
                            icon("info") %>%
                              bs_embed_tooltip(title = "If you want to train model with our MAE/BAE classification, select human or mouse option")
                          ),
                        fileInput(
                          inputId = 'tg2',
                          label = "Alternatively, upload a training file not packaged with MaGIC",
                          accept = acceptable_file_types
                        ) %>% 
                          shinyInput_label_embed(
                            icon("info") %>%
                              bs_embed_tooltip(title = "If you have your own MAE/BAE classification for training, select this file here, see documentation for the file format")
                          ),
                        textInput('targetFeature', "Target Feature (the name of the column with class labels, normally 'status')",
                                  placeholder = 'Enter name of target column',
                                  value="status"
                        ),
                        numericInput("trainingPercent",
                                     "Fraction of Data to use for Training:", 80,
                                     min = 0, max = 100, step = 10),
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
                        fileInput('validationSet', 'Upload an external validation set if you have one:',
                                  accept = acceptable_file_types) %>% 
                          shinyInput_label_embed(
                            icon("info") %>%
                              bs_embed_tooltip(title = "Please see documentation for the file format")
                          ),
                        actionButton("generateModelsButton", "Generate models", width = "100%"),
                        br(),
                        tags$p(HTML("<b>Now, to visualize your results, press the button below:</b>")),
                        conditionalPanel("input.generateModelsButton",
                                         actionButton("plotModelsButton", "Generate model plots and tables", width = "100%")
                        )
               ),
               tabPanel("Existing",
                        fluidRow(
                          column(12,
                                 selectizeInput(
                                   'models', 'Select Models',
                                   choices = model_names, 
                                   multiple = TRUE
                                 )
                                 # ,
                                 # tags$p(HTML("<b>Or select folder containing models</b>")),
                                 # shinyDirButton("modelDir", "Choose directory with models", "Upload"),
                                 # model_name <- get_names("input.modelDir", pattern = "*.rds"),
                                 # tags$p(HTML("<br>")),
                                 # selectizeInput(
                                 #   'models', 'When the lodder is selected, select Models',
                                 #   choices = model_name, multiple = TRUE
                                 # )
                          ))
               ))
           )),
         conditionalPanel("output.generate_done == TRUE",
                          fluidRow(column(12,
                                          actionButton("next_analyze", "Next"),
                                          tags$style(type="text/css", "#next_analyze { width:10%; margin-left: 1000px;}")
                          ))
         )
)
