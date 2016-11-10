
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
acceptable_file_types <- c("text/plain",
  "text/csv", 
  "text/comma-separated-values", 
  ".csv",
  "text/tsv", 
  "text/tab-separated-values",
  ".tsv")
sampling_types <- c("none", "down", "up")

######
### UI
######


shinyUI(
  navbarPage("MaGIC 2.0",
             theme = shinytheme("cosmo"),
             
  tabPanel("Processing",
           sidebarLayout(
             sidebarPanel(
               p(strong("Input directory")),
               directoryInput("processingDirButton", label = "select input directory",
                              value = "~"),
               #actionButton("processingDirButton", "Select directory"),
               p(),
               selectizeInput(
                 'refseq', 'Refseq file',
                 choices = refseq_reference
                 ),
               numericInput("dropPercent", "Drop percent:", 0.01, 
                            min = 0, max = 0.9999, step = 0.01),
               numericInput("promoterLength", "Promoter length:", 5000, min = 0,
                            step = 100),
               checkboxInput("disableFilter", 
                             "Disable filtering?",
                             value = FALSE),
               checkboxInput("noOverlap", 
                             "Disable overlap calculation?",
                             value = FALSE),
               actionButton("processDataButton", "Process data", width = "100%")
               ),
             mainPanel(
               verbatimTextOutput("processingDir")
               )
             )
           ),
  tabPanel("Analysis",
           sidebarLayout(
             sidebarPanel(
               p(strong("Input directory")),
               actionButton("analysisDirButton", "Select input directory"),
               p()
             ),
             mainPanel(
               verbatimTextOutput("analysisDir")
               )
             )
           ),
  tabPanel("Model Generation",
           sidebarLayout(
             sidebarPanel(
               fileInput('trainingFile', 'Choose input file',
                         accept = acceptable_file_types),
               selectizeInput(
                 'samplingMethod', 'Sampling method',
                 choices = sampling_types
               )
             ),
             mainPanel(
               verbatimTextOutput("analysisDir")
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

