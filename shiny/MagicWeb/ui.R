
######
### UI LIBRARIES
######


library(shiny)
library(markdown)
library(shinythemes)

######
### UI GLOBALS
######


# All ui-specific global variables
refseq_reference <- c("mm9", "hg19")
tg_names <- get_names(reference_folder, pattern = "*_tg.tsv")
tg_names <- c(tg_names, "NONE")
model_names <- get_names(models_folder, pattern = "*_model.rds")
acceptable_file_types <- c("text/plain",
  "text/csv", 
  "text/comma-separated-values", 
  ".csv",
  "text/tsv", 
  "text/tab-separated-values",
  ".tsv")
sampling_types <- c("none", "down", "up")
selection_rules <- c("best", "oneSE", "tolerance")

######
### UI
######


shinyUI(tagList(
  navbarPage("MaGIC 2.0",
             theme = shinytheme("cosmo"),
             
  tabPanel("Processing",
           sidebarLayout(
             sidebarPanel(
               directoryInput("processingDir", label = "Select Input Directory",
                              value = "~"),
               p(),
               selectizeInput(
                 'refseq', 'Select Refseq',
                 choices = refseq_reference
                 ),
               selectizeInput(
                 'tg', 'Select Training Genes',
                 choices = tg_names,
                 selected = "NONE"
               ),
               numericInput("dropPercent", "Drop Percent:", 0.01, 
                            min = 0, max = 0.9999, step = 0.01),
               numericInput("promoterLength", "Promoter Length:", 5000, min = 0,
                            step = 100),
               checkboxInput("disableFilter", 
                             "Disable Filtering?",
                             value = FALSE),
               checkboxInput("noOverlap", 
                             "Disable Overlap Calculation?",
                             value = FALSE),
               actionButton("processDataButton", "Process data", width = "100%")
               ),
             mainPanel(
               )
             )
           ),
  tabPanel("Analysis",
           sidebarLayout(
             sidebarPanel(
               fileInput('analysisFile', 'Upload Processed TSV File',
                         accept = acceptable_file_types),
               selectizeInput(
                 'models', 'Select Models', 
                 choices = model_names, multiple = TRUE
               ),
               textInput('positiveClass', 'Positive Class',
                         placeholder = 'Enter name of positive class'),
               actionButton("analyzeDataButton", "Analyze data", width = "100%")
             ),
             mainPanel(
               )
             )
           ),
  tabPanel("Model Generation",
           sidebarLayout(
             sidebarPanel(
               fileInput('trainingFile', 'Upload Processed TSV File',
                         accept = acceptable_file_types),
               textInput('targetFeature', 'Target Feature',
                         placeholder = 'Enter name of target column'),
               selectizeInput(
                 'samplingMethod', 'Sampling Method',
                 choices = sampling_types
               ),
               selectizeInput(
                 'selectionRule', 'Selection Rule',
                 choices = selection_rules
               ),
               actionButton("generateModelsButton", "Generate models", width = "100%")
             ),
             mainPanel(
             )
           )
  ),
  tabPanel("Usage",
           fluidRow(
             column(12,
                    includeMarkdown("usage.md")
                    )
             )
           ),
  tabPanel("About",
           fluidRow(
             column(12,
                    includeMarkdown("about.md")
                    )
             )
           )
  )
))

