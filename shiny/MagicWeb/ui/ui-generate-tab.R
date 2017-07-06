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
          title = ">Generate",
         sidebarLayout(
           fluidRow(column(6,
                           radioButtons("choosePath",
                                       "Choose a model",
                                       choices = c("Generate new model",
                                                    "Use existing model"),
                                       selected = "Use existing model"
                                      )
           )),
           conditionalPanel(
             condition = "input.choosePath == 'Generate new model'",
             sidebarPanel(
               fileInput('trainingFile', 'Upload Processed TSV File',
                         accept = acceptable_file_types),
               textInput('targetFeature', 'Target Feature',
                         placeholder = 'Enter name of target column'),
               numericInput("trainingPercent",
                            "Fraction of Data to use for Training:", 0.01,
                            min = 0, max = 0.9999, step = 0.01),
               selectizeInput(
                 "modelList",
                 "Select models to train",
                 choices = model_list,
                 multiple=TRUE
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
               actionButton("generateModelsButton", "Generate models", width = "100%")
             ),
             mainPanel(
               htmlOutput("generateText"),
               conditionalPanel(condition = "output.generate_output",
                                downloadButton("downloadGenerateButton",
                                               "Download generated models"))
             )
           )
         ),
         fluidRow(
           column(6,
          conditionalPanel(
           condition = "input.choosePath == 'Use existing model'",
             selectizeInput(
               'models', 'Select Models',
               choices = model_names, multiple = TRUE
             ),
           h2(HTML("<u>Or:</u>")),
           directoryInput("modelDir",
             label = "Select model directory:",
             value = "~"),
           conditionalPanel(
             h3(HTML("Now:")),
             condition = "input.modelDir",
             model_name <- get_names("input.modelDir", pattern = "*.rds"),
             selectizeInput(
               'models', 'Select Models',
               choices = model_name, multiple = TRUE
             )
           )
           )
          )),
         fluidRow(column(12,
         actionButton("next_analyze", "Next"),
         tags$style(type="text/css", "#next_analyze { width:10%; margin-left: 1000px;}")
         ))
)