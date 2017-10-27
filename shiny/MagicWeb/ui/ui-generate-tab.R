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
                         )
                         # tabPanel("Precision-recall plot",
                         #              plotOutput("modelPlot",
                         #                         height = 480,
                         #                         width = 700
                         #              )
                         #              
                         #            )
              )
              ),
              sidebarPanel = sidebarPanel(
              tabsetPanel(
                id = "options",
                tabPanel("New", 
               tags$h4(HTML("<u>Select model output directory</u>")),
               shinyDirButton("generateOutput", "Choose output directory", "Upload"),
               br(),
               br(),
               fileInput('trainingFile', 'Upload Processed ChIP-seq TSV File',
                         accept = acceptable_file_types),
               fileInput('validationSet', 'Upload an external validation set if you have one (optional):',
                         accept = acceptable_file_types),
               selectizeInput(
                 inputId = 'tg', 
                 label = 'Select training genes',
                 choices = tg_names,
                 selected = 'none'
               ),
               fileInput(
                 inputId = 'tg2',
                 label = "Alternatively, select a training file not packaged with MaGIC",
                 accept = acceptable_file_types
               ),
               textInput('targetFeature', "Target Feature (the name of the column with class labels, normally 'status'",
                         placeholder = 'Enter name of target column'),
               numericInput("trainingPercent",
                            "Fraction of Data to use for Training:", 80,
                            min = 0, max = 100, step = 10),
               selectizeInput(
                 "modelList",
                 "Select models to train",
                 choices = model_list,
                 multiple=TRUE
               ), 
               textInput("positiveClass",
                           label = "Enter positive class (the class label being predicted, normally 'MAE')"
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
                 choices=c(1:20)
               ),
               actionButton("generateModelsButton", "Generate models", width = "100%"),
               br(),
               h4( HTML("Now, to visualize your results, press the button below.")),
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
             ),
           h4(HTML("<u>Or:</u>")),
           tags$h4(HTML("<u>Select folder containing models</u>")),
           shinyDirButton("modelDir", "Choose directory with models", "Upload"),
             h3(HTML("Now:")),
             model_name <- get_names("input.modelDir", pattern = "*.rds"),
             selectizeInput(
               'models', 'Select Models',
               choices = model_name, multiple = TRUE
             )
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
