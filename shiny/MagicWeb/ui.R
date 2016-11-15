
######
### UI LIBRARIES
######


library(shiny)
library(markdown)
library(shinythemes)

######
### UI FUNCTIONS
######


# Loads all model names in a given folder into a list
get_models <- function(input_folder) {
  models <- list()
  model_files <- list.files(input_folder, pattern = "*_model.rds", recursive = FALSE)
  for (file in model_files) {
    models <- c(models, strsplit(file, "_")[[1]][1])
  }
  return(models)
}

######
### UI GLOBALS
######


# All ui-specific global variables
refseq_reference <- c("mm9", "hg19")
model_names <- get_models(models_folder)
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


shinyUI(
  navbarPage("MaGIC 2.0",
             theme = shinytheme("cosmo"),
             
  tabPanel("Processing",
           sidebarLayout(
             sidebarPanel(
               directoryInput("processingDir", label = "Select Input Directory",
                              value = "~"),
               p(),
               selectizeInput(
                 'refseq', 'Select Refseq File',
                 choices = refseq_reference
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
                 'models', 'Select Models', choices = model_names, multiple = TRUE
               ),
               textInput('positiveClass', 'Positive Class',
                         placeholder = 'Enter name of positive class')
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
               )
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
)

