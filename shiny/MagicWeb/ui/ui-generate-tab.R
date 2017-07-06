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