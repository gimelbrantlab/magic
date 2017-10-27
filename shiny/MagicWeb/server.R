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
### SERVER LIBRARIES AND SCRIPTS
######


######
### SERVER GLOBALS
######

process_file <- file.path(src_folder, "process.R")
generate_file <- file.path(src_folder, "generate.R")
analyze_file <- file.path(src_folder, "analyze.R")
model_names <- get_names(models_folder, pattern = "*_model.rds")

######
### SERVER
######

shinyServer(function(input, output, session) {
  
  # Session variables
  temp_dir <- NULL
  no_overlap <- NULL
  
  # new process arguments
  process_dir <- NULL
  no_filter_olf <- NULL
  no_filter_chrom <- NULL
  no_filter_imprinted <- NULL
  promoter_length <- NULL
  refseq_name <- NULL
  drop_percent <- NULL
  cores <- NULL
  training_genes <- NULL
  
  # new generate arguments
  model_list <- NULL
  positive_class <- NULL
  excluded_models <- NULL
  target_feature <- NULL
  sampling_method <- NULL
  selection_rule <- NULL
  
  # analysis arguments
  analysis_file_path <- NULL
  
  # Session variables for program output
  process_output <- NULL
  process_running <- FALSE
  analyze_output <- NULL
  analyze_running <- FALSE
  generate_output <- NULL
  generate_running <- FALSE
  
  ### VARIABLE UPDATING

  positive_class <- reactive({ input$positiveClass })
  target_feature <- reactive({ input$targetFeature })
  sampling_method <- reactive({ input$samplingMethod })
  selection_rule <- reactive({ input$selectionRule })
  excluded_models <- reactive({ paste(model_names[!(model_names %in% input$models)],
                                      sep = ",", collapse = "") })
  
  
  # example shinyFiles
  shinyDirChoose(input, 'dir', roots = c(home = '~'), filetypes = c('', 'txt','bigWig',"tsv","csv","bw"))
  dir <- reactive(input$dir)
  output$dir <- renderPrint(dir())
  
  # path
  # datapath <<- reactive({
  #   home <- normalizePath("~")
  #   file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
  # })
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$dir
    },
    handlerExpr = {
        home <- normalizePath("~")
        datapath <<- file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
    }
  )
  
  # get data path for passing to process.R
  # observeEvent(
  #   ignoreNULL = TRUE,
  #   eventExpr = {
  #     input$dataDirectory
  #   },
  #   handlerExpr = {
  #     # prevent launching directoryInput at app start
  #     if (input$dataDirectory){
  #       datapath <<- choose.dir(default = readDirectoryInput(session, 'dataDirectory'))
  #     }
  #   }
  # )
  
  # get output path for passing to  process.R
  # observeEvent(
  #   ignoreNULL = TRUE,
  #   eventExpr = {
  #     input$outputDirectory
  #   },
  #   handlerExpr = {
  #     if (input$outputDirectory){
  #       output_path <<- choose.dir(default = readDirectoryInput(session, "outputDirectory"))
  #       
  #     }
  #   }
  # )
  
  shinyDirChoose(input, 'outputPath', roots = c(home = '~'), filetypes = c('', 'txt','bigWig',"tsv","csv","bw","rds"))
  outputPath <- reactive(input$outputPath)
  output$outputPath <- renderPrint(outputPath())
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$outputPath
    },
    handlerExpr = {
        home <- normalizePath("~")
        output_path <<- file.path(home, paste(unlist(outputPath()$path[-1]), collapse = .Platform$file.sep))
    }
  )
  

  
  source("server/server-main.R", local=TRUE)$value
  source("server/server-analyze.R", local=TRUE)$value
  source("server/server-generate.R", local=TRUE)$value
  source("server/server-process.R", local=TRUE)$value
})

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



# var_update <- reactive({
#   refseq_name <- input$refseq
#   training_genes <- input$tg
#   drop_percent <- input$dropPercent
#   promoter_length <- input$promoterLength
#   disable_filtering <- input$disableFilter
#   no_overlap <- input$noOverlap
#   excluded_models <- paste(model_names[!(model_names %in% input$models)],
#                            sep = ",", collapse = "")
#   positive_class <- input$positiveClass
#   target_feature <- input$targetFeature
#   sampling_method <- input$samplingMethod
#   selection_rule <- input$selectionRule
# })

#   ### DATA PROCESSING
#   
#   # Gets selected input folder
#   observeEvent(
#     ignoreNULL = TRUE,
#     eventExpr = {
#       input$processingDir
#     },
#     handlerExpr = {
#       if (input$processingDir > 0) {
#         temp_dir <- choose.dir(default = readDirectoryInput(session, 'processingDir'))
#         updateDirectoryInput(session, 'processingDir', value = temp_dir)
#       }
#     }
#   )
#   
# # action buttons between tabs
#   observeEvent(
#     input$get_started,
#     {updateTabsetPanel(session = session, inputId = "main_panel", selected = "input")}
#   )
#   
#   
#   observeEvent(
#    input$next_process,
#     {updateTabsetPanel(session = session, inputId = "main_panel", selected = "process")}
#   )
#   
#   observeEvent(
#     input$next_generate,
#     {updateTabsetPanel(session = session, inputId = "main_panel", selected = "generate")}
#   )
#   
#   observeEvent(
#     input$next_analyze,
#     {updateTabsetPanel(session = session, inputId = "main_panel", selected = "analyze")}
#   )
#   # Processes data on button press
#   observeEvent(input$processDataButton, {
#     
#     # Gets processing directory from widget
#     process_dir <- readDirectoryInput(session, 'file_input')
#     
#     # Builds command to run process.R and executes it
#     if (!is.null(process_dir)) {
#       process_output <- NULL
#       process_running <- TRUE
#       process_cmd <- paste("Rscript")
#       args <- paste(process_file,
#                     "-i", process_dir,
#                     "-o", output_folder,
#                     "-p", promoter_length(),
#                     "-d", drop_percent(),
#                     "-r", refseq_name(),
#                     "-t", training_genes())
#       if(cores()) { args <- paste(args, "-s") }
#       if(no_filter_olf()) { args <- paste(args, "-f") }
#       if (disable_filtering()) { args <- paste(args, "-f") }
#       if (no_overlap()) { args <- paste(args, "-l") }
#       cat("\n", args, "\n\n")
#       process_output <- capture.output(tryCatch(
#         system2(process_cmd, args), error = function(e) e))
#     }
#   })
#   
#   
#   # Sets reactive processing output text
#   output$processText <- renderUI({
#     
#     line_1 <- "Processing text goes here"
#     line_2 <- ""
#     line_3 <- ""
#     
#     if(process_running) { line_2 <- "Processing data..." }
#     
#     if(!is.null(process_output)) {
#       process_running <- FALSE
#       line_3 <- process_output
#     }
#     
#     HTML(paste(line_1, line_2, line_3, sep = "<br/>"))
#   })
#   
#   # Handler for processed data download
#   output$downloadProcessButton <- downloadHandler(
#     filename = function() { paste("processedData.txt") } ,
#     content <- function(file) {
#       df <- read.csv(file.path(output_folder, "joined_scores_percentile.txt"),
#                      sep = "\t")
#       write.table(df, file, sep = "\t", row.names = FALSE, quote = FALSE)
#     }
#   )
#   
#   # Output data table
#   output$procesTable <- 
#   
#   # Output plots about data
#   output$processPlots <- renderPlot({
#     
#     ggplot(outpu)
#     
#     
#   })


# ### DATA ANALYSIS
# 
# # Gets uploaded analysis file
# getAnalysisFile <- reactive({
#   if(!is.null(input$analysisFile)) { 
#     analysis_file_path <- input$analysisFile$datapath 
#   }
# })
# 
# # Analyzes data on button press
# observeEvent(input$analyzeDataButton, {
#   
#   # Builds command to run analyze.R and executes it
#   if ((!is.null(analysis_file_path)) &&
#       (!is.null(models)) && 
#       (!is.null(positive_class))) {
#     analyze_output <- NULL
#     analyze_running <- TRUE
#     analyze_cmd <- paste("Rscript")
#     args <- paste(analyze_file,
#                   "-i", analysis_file_path,
#                   "-m", models_folder,
#                   "-o", output_folder,
#                   "-ex", excluded_models,
#                   "-p", positive_class)
#     analyze_output <- capture.output(tryCatch(
#       system2(analyze_cmd, args), error = function(e) e))
#   }
# })
# 
# # Sets reactive analysis output text
# output$analysisText <- renderUI({
#   
#   line_1 <- ""
#   line_2 <- ""
#   
#   if(analyze_running) { line_1 <- "Analyzing data..." }
#   
#   if(!is.null(analyze_output)) {
#     analyze_running <- FALSE
#     line_2 <- analyze_output
#   }
#   
#   HTML(paste(line_1, line_2, sep = "<br/>"))
# })
# 
# # Handler for analyzed data download
# output$downloadAnalyzeButton <- downloadHandler(
#   filename = function() { paste("analyzedData.txt") } ,
#   content <- function(file) {
#     df <- read.csv(file.path(output_folder, "all_predictions.tsv"),
#                    sep = "\t")
#     write.table(df, file, sep = "\t", row.names = FALSE, quote = FALSE)
#   }
# )

### MODEL GENERATION 

# Gets uploaded training file
#   getAnalysisFile <- reactive({
#     if(!is.null(input$trainingFile)) { 
#       training_file_path <- input$trainingFile$datapath 
#     }
#   })
#   
#   # Generates models on button press
#   observeEvent(input$generateModelsButton, {
#     
#     # Builds command to run generate.R and executes it
#     if ((!is.null(training_file_path)) &&
#         (!is.null(target_feature))) {
#       generate_output <- NULL
#       generate_running <- TRUE
#       generate_cmd <- paste("Rscript")
#       args <- paste(generate_file,
#                     "-i", training_file_path,
#                     "-o", output_folder,
#                     "-m", sampling_method,
#                     "-r", selection_rule,
#                     "-t", target_feature)
#       generate_output <- capture.output(tryCatch(
#         system2(generate_cmd, args), error = function(e) e))
#     }
#   })
#   
#   # Sets reactive generation output text
#   output$generateText <- renderUI({
#     
#     line_1 <- ""
#     line_2 <- ""
#     
#     if(generate_running) { line_1 <- "Generating models..." }
#     
#     if(!is.null(generate_output)) {
#       generate_running <- FALSE
#       line_2 <- generate_output
#     }
#     
#     HTML(paste(line_1, line_2, sep = "<br/>"))
#   })
