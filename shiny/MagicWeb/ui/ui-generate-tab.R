tabPanel(value = "generate",
          title = "Select model",
         sidebarLayout(
              # mainPanel = mainPanel(NULL),
          mainPanel = mainPanel(
             tabsetPanel(
              id = "generatePlots",
                         tabPanel("Summary Table",
                                dataTableOutput("modelTbl")
                         ),
                         tabPanel("Precision-recall plot",
                                      plotOutput("modelPlot",
                                                 height = 480,
                                                 width = 700
                                      )
                                      
                                    ))
              ),
              sidebarPanel = sidebarPanel(
              tabsetPanel(
                id = "options",
                tabPanel("New",   
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
               textInput('targetFeature', 'Target Feature (the name of the column with class labels, normally MAE/BAE)',
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
           h3(HTML("<u>Or:</u>")),
           directoryInput(
             "modelDir",
             label = "Select model directory:",
             value = "~"),
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
