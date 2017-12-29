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

################
# ANALYZE SERVER
################

# sets output path
shinyDirChoose(input, 'analyzeOutput', roots = c(home = '~'), filetypes = c('', 'txt','bigWig',"tsv","csv","bw", "rds"))
analyzeOutput <- reactive(input$analyzeOutput)
output$analyzeOutput <- renderText({parseDirPath(c(home = '~'), analyzeOutput())})

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$analyzeOutput
  },
  handlerExpr = {
    home <- normalizePath("~")
    output_analyze <<- file.path(home, paste(unlist(analyzeOutput()$path[-1]), collapse = .Platform$file.sep))
    print(output_analyze)
  }
)

# sets models folder path
shinyDirChoose(input, 'modelFolder', roots = c(home = '~'), filetypes = c('', 'txt','rds'))
modelFolder <- reactive(input$modelFolder)
output$modelFolder <- renderText({parseDirPath(c(home = '~'), modelFolder())})

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$modelFolder
  },
  handlerExpr = {
    home <- normalizePath("~")
    models_folder <<- file.path(home, paste(unlist(modelFolder()$path[-1]), collapse = .Platform$file.sep))
    print(models_folder)
  }
)


### DATA ANALYSIS

# Gets uploaded analysis file
getAnalysisFile <- reactive({
  if(!is.null(input$analysisFile)) {
    analysis_file_path <<- input$analysisFile$datapath
  }
})

# Gets expression filter file
# getExpressionFile <- reactive({
#   if(!is.null(input$expressionData)) {
#     if (input$filterFile == 'custom') {
#       expression_file_path <<- input$expressionData$datapath
#     }
#     else {
#       if (input$filterFile == 'human') {
#         expression_file_path <<- paste0(reference_folder,"hg19_length.txt")
#       }
#       if (input$filterFile == 'mouse') {
#         expression_file_path <<- paste0(reference_folder,"mm10_length.txt")
#       }
#     }
#   }
#   else {
#     if (input$filterFile == 'human') {
#       expression_file_path <<- paste0(reference_folder,"hg19_length.txt")
#     }
#     if (input$filterFile == 'mouse') {
#       expression_file_path <<- paste0(reference_folder,"mm10_length.txt")
#     }
#     if (input$filterFile == 'custom') {
#       showModal(modalDialog(
#         title = "Error",
#         "Please, upload file with expression and/or lengths, or select human or mouse",
#         easyClose = TRUE
#       ))
#     }
#   }
# })

# Analyzes data on button press


observeEvent(input$analyzeDataButton,
             {
               message_list <- c("Preparing analyze command","Loading files","Running analyze.R")
               withProgress(value = 0,
                            {
                              for (i in 1:length(message_list)){
                                incProgress(1/length(message_list), detail = paste(message_list[i]))
                                Sys.sleep(0.25)
                              }
                              if(dir.exists(paste(models_folder))){
                                filenames <- Sys.glob(file.path(models_folder, "*_model.rds"))
                                if (length(filenames)>0) {
                                  # get filter file
                                  if(!is.null(input$expressionData)) {
                                    if (input$filterFile == 'custom') {
                                      expression_file_path <- input$expressionData$datapath
                                    }
                                    else {
                                      if (input$filterFile == 'human') {
                                        expression_file_path <- paste0(reference_folder,"/hg19_length.txt")
                                      }
                                      if (input$filterFile == 'mouse') {
                                        expression_file_path <- paste0(reference_folder,"/mm10_length.txt")
                                      }
                                    }
                                  }
                                  else {
                                    print("here we are")
                                    if (input$filterFile == 'human') {
                                      expression_file_path <- paste0(reference_folder,"/hg19_length.txt")
                                    }
                                    if (input$filterFile == 'mouse') {
                                      expression_file_path <- paste0(reference_folder,"/mm10_length.txt")
                                    }
                                    if (input$filterFile == 'custom') {
                                      showModal(modalDialog(
                                        title = "Error",
                                        "Please, upload file with expression and/or lengths, or select human or mouse",
                                        easyClose = TRUE
                                      ))
                                    }
                                    print(expression_file_path)
                                  }
                                  # Builds command to run analyze.R and executes it
                                  analyze_output <- NULL
                                  analyze_running <- TRUE
                                  analyze_cmd <- paste("Rscript")
                                  args <- paste(analyze_file,
                                                "-i", input$analysisFile$datapath,
                                                "-m", paste(models_folder),
                                                "-o", paste(output_analyze, "/analysis_output", sep=""),
                                                "-p", "MAE")
                                  if((!is.null(input$exprFilt))|(!is.null(input$lengthFilt))) { args <- paste(args, "-f", expression_file_path) }
                                  if(!is.null(input$lengthFilt)) { args <- paste(args, "-l", input$lengthFilter) }
                                  cat(args)
                                  analyze_output <- capture.output(tryCatch(
                                    system2(analyze_cmd, args), error = function(e) e))
                                  predictTable <- load_data(paste(output_analyze, "/analysis_output/all_predictions.tsv", sep=""))
                                  predictTable %>% dplyr::select(name, grep("_predictions", colnames(predictTable))) -> predictTable
                                  output$predTbl <- renderDataTable(
                                    predictTable
                                  )
                                }
                                else {
                                  showModal(modalDialog(
                                    title = "Error",
                                    "Your model folder doesn't contain any models (named *_model.rds), please select folder with models and rerun",
                                    easyClose = TRUE
                                  ))
                                }
                              }
                              else {
                                showModal(modalDialog(
                                  title = "Error",
                                  "Your model folder doesn't exist, please select folder with models and rerun",
                                  easyClose = TRUE
                                ))
                              }
                            })
             })

