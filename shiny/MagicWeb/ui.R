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

       
######
### UI LIBRARIES
######

library(shiny)
library(markdown)
library(shinythemes)
library(GGally)
library(shinyFiles)

######
### UI GLOBALS
######

# All ui-specific global variables
organism <- c("human", "mouse", "other")
assembly <- c("mm9","mm10", "other")
assembly <- c(assembly, "hg19", "hg38", "other")
tg_names <- get_names(reference_folder, pattern = "*_tg.tsv")
tg_names <- c(tg_names, "none", "other")
model_names <- get_names(models_folder, pattern = "*_model.rds")
acceptable_file_types <- c("text/plain",
  "text/csv",
  "text/comma-separated-values",
  ".csv",
  "text/tsv",
  "text/tab-separated-values",
  ".tsv")
selection_rules <- c("best", "oneSE", "tolerance")
metric_names <- c("Kappa", "Accuracy", "ROC")
sampling_method_names <- c("none", "down", "up")
positive_classes <- c("MAE", "BAE", "other")
model_list <- c("ada", "svmPoly", "rf", "nnet", "gmlboost", "rpart", "mlpML", "knn", "evtree", "glmStepAIC")
filtering <- c("olfactory genes", "sex chromosomes", "imprinted genes")

source("../../src/utils.R", local=TRUE)
load_process_libraries()
load_analyze_libraries()
load_generate_libraries()
load_shiny_libraries()
######
### UI
######


shinyUI(
  
  tagList(
    # make navbar look cool
    navbarPage(
       title = "", id="main_panel",
       # good themes: flatly, simplex
       theme = shinytheme("flatly"),
    
     # source tabPanels
     source("ui/ui-main-tab.R", local=TRUE)$value,
     # source("ui/ui-input-tab.R", local=TRUE)$value,
     source("ui/ui-process-tab.R", local=TRUE)$value,
     source("ui/ui-generate-tab.R", local=TRUE)$value,
     source("ui/ui-analyze-tab.R", local=TRUE)$value,
     
     # Additional information about the program
     navbarMenu("More",
     source("ui/ui-cmd-tab.R", local=TRUE)$value,
     source("ui/ui-about-tab.R", local=TRUE)$value
     )
  )
)
)



##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################





# tabPanel(value = "main",
#          title = p("MaGICs", style = "font-size: 20px; padding-bottom: -0.5cm"),
#          fluidRow(column(12,
#          sidebarLayout(
#            sidebarPanel(
#              h2(HTML("Welcome to MaGICs: <br> <b>M</b>ono<b>a</b>llelic <b>G</b>ene <b>I</b>nference from <b>C</b>hromatin for <b>S</b>hiny")),
#              actionButton("get_started", "Get started"),
#              br(),
#              h2(HTML("<u>Overview</u>")),
#              tags$p(HTML(
#                "MaGIC 2.0 is a graphical user interface for predicting allele specific expression in
#                polyclonal cell samples. The program leverages the predictive power of chromatin signatures
#                to generate educated inferences on monoallelic state of genes within a polyclonal sample via
#                statistical learning methods and monoclonal allelic bias determination.")),
#              p(),
#              h4(
#                HTML("Now, choose a format to provide MaGIC with your data. <br>")
#                ),
#                  tags$p(HTML("<b>Files</b> takes a correctly formatted '.txt' or '.tsv' file with names of chromatin marks
#                   and control files and automatically selects the correct files from the input directory. <br><br>
#                   <b>Fields</b> creates fields for inputting mark names into the interface.")
#                ),
#              h3(
#                HTML("MaGIC offers three distinct functionalities: <br>")
#              ),
#              h4(
#                HTML("1. Process")
#              ),
#              tags$p(HTML("Process is a shiny wrapper for the command line utility <b>process.R</b>.
#                          This program functions to convert bigWig data from processed ChIP-Seq reads and
#                          convert them to mapped scores compatible with analysis of MAE status through
#                          analyze.R and model generation through generate.R. Process directly passes the
#                          bigWig files inputted to this page and outputs *.tsv* files with ChIP-Seq region
#                          scores by gene. Navigate to the <b>Processing</b> tab to use this feature.")),
#              h4(
#                HTML("2. Analyze")
#              ),
#              tags$p(HTML("Analyze is a shiny wrapper for the command line utility <b>analyze.R</b>.
#                      This program functions to provide predictions for a sample using a
#                      previously trained model to determine status for genes within that sample. Simply provide
#                      a file of ChIP-Seq enrichment scores from process.R and retrieve predictions.
#                      Navigate to the <b>Analysis</b> tab to use this feature.")), 
#              h4(
#                HTML("3. Generate")
#              ),
#              tags$p(HTML("We encourage the use of MaGIC 2.0 to improve upon published predictive models
#                          for inferring MAE to improve predictive reliability and indicate possible mechanistic
#                          implications of epigenetic signatures in MAE. To this end, Generate is a shiny wrapper
#                          for the command line utility <b>generate.R</b>. This program functions to allow users to
#                          input training data based on monoclonal allelic imbalance data and novel epigenetic signature
#                          data to create models with a variety of different algorithms to further optimize the effort to
#                          characterize cell-cell allele-specific heterogeneity. Navigate to the <b>Model Generation</b> tab to use
#                          this feature.")),
#              width = 12
#              ),
#            mainPanel(img(src='opening_slide.png', align='left', width='900px', height='530px')),
#            position = "left"
#            )
#          #### File method
#            )
#          )),

# tabPanel(value = "input",
#          title = "Input",
#        sidebarLayout(
#          sidebarPanel(
#            br(),
#            h4( HTML("First, set your working directory that contains all your input data.")),
#            directoryInput("processingDir",
#                           label = "Select input directory:",
#                           value = "~"),
#           width = 8
#          ),
#          mainPanel(
#        fluidRow(column(12,
#               tabsetPanel(
#                 tabPanel("Option 1: File",
#                          column(12,
#                                 sidebarLayout(
#                                   fluidRow(
#                                     mainPanel(
#                                       tags$h3(HTML("<u>Input to Processing</u>")),
#                                       p(HTML("To use this method input a file following the format below with fields seperated by tabs
#                                              in *.txt* or *.tsv* format:<br>
#                                              <table style='width:100%'>
#                                              <tr>
#                                              <th>mark name</th>
#                                              <th>mark file</th>
#                                              <th>control file</th>
#                                              </tr>
#                                              <tr>
#                                              <td>mark1</td>
#                                              <td>input_dir/sample_mark1.bigWig</td>
#                                              <td>input_dir/control_mark1.bigWig</td>
#                                              </tr>
#                                              <tr>
#                                              <td>mark2</td>
#                                              <td>input_dir/sample_mark2.bigWig</td>
#                                              <td>input_dir/sample_mark2.bigWig</td>
#                                              </tr>
#                                              </table>")),
#                                       br(),
#                                       p("The program will now automatically pick the filenames you provided when you
#                                         move to the processing tab."),
#                                       width = 12
#                                       )
#                                       ),
#                                   fluidRow(
#                                     sidebarPanel(
#                                       fluidRow(fileInput(
#                                         'file_inputs',
#                                         label = h5("Upload file"),
#                                         accept = c(
#                                           'text/comma-separated-values',
#                                           'text/tab-separated-values',
#                                           '.tsv'
#                                         )
#                                       )),
#                                       width = 12
#                                     )
#                                   )))),
#                 
#                 ### Field method
#                 tabPanel("Option 2: Fields",
#                          column(12,
#                                 sidebarLayout(
#                                   fluidRow(
#                                     mainPanel(
#                                       tags$h3(HTML("<u>Input usage</u>")),
#                                       p(HTML("To use this method, first indicate how many mark files you are evaluating,
#                                              then, simply fill in the following fields with the exact file names
#                                              of the unprocessed chromatin signature data in the provided directory.")),
#                                       width = 12
#                                       )
#                                       ),
#                                   fluidRow(
#                                     sidebarPanel(
#                                       fluidRow(numericInput(
#                                         "marks",
#                                         "Number of epigenetic signatures being evaluated:", value=0,
#                                         min = 2, max = 5
#                                       ),
#                                       conditionalPanel(condition = "input.marks == 2",
#                                                        textInput("filename1", "Mark file name of first mark", "mark1_sample.bigWig"),
#                                                        textInput("control1", "Control file name of first mark", "mark1_control.bigWig"),
#                                                        textInput("filename2", "Mark file name of second mark", "mark2_sample.bigWig"),
#                                                        textInput("control2", "Control file name of second mark", "mark2_control.bigWig")
#                                       ),
#                                       conditionalPanel(condition = "input.marks == 3",
#                                                        textInput("filename1", "Mark file name of first mark", "mark1_sample.bigWig"),
#                                                        textInput("control1", "Control file name of second mark", "mark1_control.bigWig"),
#                                                        textInput("filename2", "Mark file name of second mark", "mark2_sample.bigWig"),
#                                                        textInput("control2", "Control file name of second mark", "mark2_control.bigWig"),
#                                                        textInput("filename3", "Mark file name of third mark", "mark3_sample.bigWig"),
#                                                        textInput("control3", "Control file name of third mark", "mark3_control.bigWig")
#                                       ),
#                                       conditionalPanel(condition = "input.marks == 4",
#                                                        textInput("filename1", "Mark file name of first mark", "mark1_sample.bigWig"),
#                                                        textInput("control1", "Control file name of second mark", "mark1_control.bigWig"),
#                                                        textInput("filename2", "Mark file name of second mark", "mark2_sample.bigWig"),
#                                                        textInput("control2", "Control file name of second mark", "mark2_control.bigWig"),
#                                                        textInput("filename3", "Mark file name of third mark", "mark3_sample.bigWig"),
#                                                        textInput("control3", "Control file name of third mark", "mark3_control.bigWig"),
#                                                        textInput("filename4", "Mark file name of fourth mark", "mark4_sample.bigWig"),
#                                                        textInput("control4", "Control file name of fourth mark", "mark4_sample.bigWig")
#                                       ),
#                                       conditionalPanel(condition = "input.marks == 5",
#                                                        textInput("filename1", "Mark file name of first mark", "mark1_sample.bigWig"),
#                                                        textInput("control1", "Control file name of second mark", "mark1_control.bigWig"),
#                                                        textInput("filename2", "Mark file name of second mark", "mark2_sample.bigWig"),
#                                                        textInput("control2", "Control file name of second mark", "mark2_control.bigWig"),
#                                                        textInput("filename3", "Mark file name of third mark", "mark3_sample.bigWig"),
#                                                        textInput("control3", "Control file name of third mark", "mark3_control.bigWig"),
#                                                        textInput("filename4", "Mark file name of fourth mark", "mark4_sample.bigWig"),
#                                                        textInput("control4", "Control file name of fourth mark", "mark4_sample.bigWig"),
#                                                        textInput("filename5", "Mark file name of fifth mark", "mark5_sample.bigWig"),
#                                                        textInput("control5", "Control file name of fifth mark", "mark5_sample.bigWig")
#                                       )
#                                       ),          
#                                       width=12
#                                     )
#                                   )
#                                 ))))
#        )))
#        ),
#        fluidRow(
#        actionButton("next_process", "Next"),
#        tags$style(type="text/css", "#next_process { width:10%; margin-left: 1000px;}")
#        )
#        ),

########
# PROCESS.R
########

# tabPanel(value = "process",
#         title = ">Process",
#          sidebarLayout(
#            sidebarPanel(
#              selectizeInput(
#                'organism', 'Select organism',
#                choices = organism,
#                selected = "human"
#              ),
#              conditionalPanel(
#                condition = "input.organism == 'mouse'",
#                selectizeInput('mouse_refseq', 'Select assembly',
#                               choices = mouse_refseq_reference,
#                               selected = "mm9")
#                ),
#              conditionalPanel(
#                condition = "input.organism == 'human'",
#                selectizeInput("human_refseq", 'Select assembly',
#                               choices = human_refseq_reference,
#                               selected = "hg19")
#              ),
#              conditionalPanel(
#                condition = "input.organism == 'other'",
#                fileInput(
#                  'file1',
#                  label = h5("Upload file"),
#                  accept = c(
#                    'text/comma-separated-values',
#                    'text/tab-separated-values',
#                    '.tsv'
#                  )
#                )
#              ),
#              selectizeInput(
#                'tg', 'Select training genes',
#                choices = tg_names,
#                selected = "none"
#              ),
#              sliderInput("dropPercent", "Drop percent:", 0.01,
#                           min = 0, max = 0.80, step = 0.01),
#              numericInput("promoterLength", "Promoter Length:", 5000, min = 0,
#                           step = 100),
#              checkboxInput("disableFilter",
#                            "Disable filtering?",
#                            value = FALSE),
#              checkboxInput("noOverlap",
#                            "Disable overlap calculation?",
#                            value = FALSE),
#              actionButton("processDataButton", "Process data", width = "100%")
#              ),
#            mainPanel(
#              htmlOutput("processText"),
#              conditionalPanel(condition = "output.process_output",
#                               downloadButton("downloadProcessButton",
#                                              "Download processed data"))
#              )
#            ),
#       fluidRow(column(12,
#       actionButton("next_generate", "Next"),
#       tags$style(type="text/css", "#next_generate { width:10%; margin-left: 1000px;}")
#       )
#       )
# ),

#######
# GENERATE.R
#######

# tabPanel(value = "generate",
#           title = ">Generate",
#          sidebarLayout(
#            fluidRow(column(6, 
#                            radioButtons("choosePath", 
#                                        "Choose a model",
#                                        choices = c("Generate new model", 
#                                                     "Use existing model"),
#                                        selected = "Use existing model"
#                                       )
#            )),
#            conditionalPanel(
#              condition = "input.choosePath == 'Generate new model'",
#              sidebarPanel(
#                fileInput('trainingFile', 'Upload Processed TSV File',
#                          accept = acceptable_file_types),
#                textInput('targetFeature', 'Target Feature',
#                          placeholder = 'Enter name of target column'),
#                numericInput("trainingPercent",
#                             "Fraction of Data to use for Training:", 0.01,
#                             min = 0, max = 0.9999, step = 0.01),
#                selectizeInput(
#                  "modelList",
#                  "Select models to train",
#                  choices = model_list,
#                  multiple=TRUE
#                ),
#                selectizeInput(
#                  'metric', 'Select Loss Function',
#                  choices = metric_names
#                ),
#                selectizeInput(
#                  'samplingMethod', 'Sampling Method',
#                  choices = sampling_method_names
#                ),
#                selectizeInput(
#                  'selectionRule', 'Selection Rule',
#                  choices = selection_rules
#                ),
#                actionButton("generateModelsButton", "Generate models", width = "100%")
#              ),
#              mainPanel(
#                htmlOutput("generateText"),
#                conditionalPanel(condition = "output.generate_output",
#                                 downloadButton("downloadGenerateButton",
#                                                "Download generated models"))
#              )
#            )
#          ),
#          fluidRow(
#            column(6,
#           conditionalPanel(
#            condition = "input.choosePath == 'Use existing model'",
#              selectizeInput(
#                'models', 'Select Models',
#                choices = model_names, multiple = TRUE
#              ),
#            h2(HTML("<u>Or:</u>")),
#            directoryInput("modelDir",
#              label = "Select model directory:",
#              value = "~"),
#            conditionalPanel(
#              h3(HTML("Now:")),
#              condition = "input.modelDir",
#              model_name <- get_names("input.modelDir", pattern = "*.rds"),
#              selectizeInput(
#                'models', 'Select Models',
#                choices = model_name, multiple = TRUE
#              )
#            )
#            )
#           )),
#          fluidRow(column(12,
#          actionButton("next_analyze", "Next"),
#          tags$style(type="text/css", "#next_analyze { width:10%; margin-left: 1000px;}")
#          ))
# ),

########
# ANALYZE.R
########


# tabPanel(value = "analyze",
#          title = ">Analyze",
#          sidebarLayout(
#            sidebarPanel(
#              fileInput('analysisFile', 'Upload processed TSV file',
#                        accept = acceptable_file_types
#                        ),
#              selectizeInput(
#                "positiveClass", 
#                "Select positive class",
#                choices = positive_classes,
#                selected = "MAE"
#                ),
#              conditionalPanel(
#                condition = "input.positiveClass == other",
#                textInput("positiveClass", 
#                label = "Enter positive class")
#               ),
#              actionButton("analyzeDataButton", "Analyze data", width = "100%")
#            ),
#            mainPanel(
#              htmlOutput("analysisText"),
#              conditionalPanel(condition = "output.analyze_output",
#                               downloadButton("downloadAnalyzeButton",
#                                              "Download analysis"))
#            ))
# ),

########
# COMMAND LINE
########
# tabPanel("Command line usage",
#          fluidRow(
#            column(12,
#                   includeMarkdown("usage.md")
#                   )
#            )
#          ),

########
# ABOUT
########
# tabPanel("About",
#          fluidRow(
#            column(12,
#                   includeMarkdown("about.md")
#                   )
#            )
#          )
