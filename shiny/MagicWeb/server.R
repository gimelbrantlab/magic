
######
### SERVER LIBRARIES AND SCRIPTS
######


library(shiny)


######
### SERVER GLOBALS
######


process_file <- file.path(src_folder, "process.R")
generate_file <- file.path(src_folder, "generate.R")
analyze_file <- file.path(src_folder, "analyze.R")
model_names <- get_names(models_folder, pattern = "*_model.rds")
output_folder <- "output"

######
### SERVER
######


shinyServer(function(input, output, session) {
  
  # Session variables
  process_dir <- NULL
  analysis_file_path <- NULL
  training_file_path <- NULL
  refseq_name <- NULL
  training_genes <- NULL
  drop_percent <- NULL
  promoter_length <- NULL
  disable_filtering <- NULL
  no_overlap <- NULL
  excluded_models <- NULL
  positive_class <- NULL
  target_feature <- NULL
  sampling_method <- NULL
  selection_rule <- NULL
  
  # Session variables for program output
  process_output <- NULL
  process_running <- FALSE
  analyze_output <- NULL
  analyze_running <- FALSE
  generate_output <- NULL
  generate_running <- FALSE
  
  ### VARIABLE UPDATING
  
  var_update <- reactive({
    refseq_name <- input$refseq
    training_genes <- input$tg
    drop_percent <- input$dropPercent
    promoter_length <- input$promoterLength
    disable_filtering <- input$disableFilter
    no_overlap <- input$noOverlap
    excluded_models <- paste(model_names[!(model_names %in% input$models)],
                             sep = ",", collapse = "")
    positive_class <- input$positiveClass
    target_feature <- input$targetFeature
    sampling_method <- input$samplingMethod
    selection_rule <- input$selectionRule
  })
  
  ### DATA PROCESSING
  
  # Gets selected input folder
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$processingDir
    },
    handlerExpr = {
      if (input$processingDir > 0) {
        process_dir = 
          choose.dir(default = readDirectoryInput(session, 'processingDir'))
        updateDirectoryInput(session, 'processingDir', value = process_dir)
      }
    }
  )
  
  # Processes data on button press
  process_data <- eventReactive(input$processDataButton, {
    
    # Builds command to run process.R and executes it
    if (!is.null(process_dir)) {
      process_output <- NULL
      process_running <- TRUE
      process_cmd <- paste("Rscript", process_dir)
      args <- paste("-i", process_dir,
                    "-o", output_folder,
                    "-f", disable_filtering,
                    "-p", promoter_length,
                    "-d", drop_percent,
                    "-l", no_overlap,
                    "-r", refseq_name,
                    "-t", training_genes)
      process_output <- capture.output(tryCatch(
        system2(process_cmd, args), error = function(e) e))
    }
  })
  
  # Sets reactive processing output text
  output$processText <- renderText({
    
    if(process_running) { paste("Processing data...") }
    
    if(!is.null(process_output)) {
      process_running <- FALSE
      paste(process_output)
    }
  })
  
  # Handler for processed data download
  output$downloadProcessButton <- downloadHandler(
    filename = function() { paste("processedData.txt") } ,
    content <- function(file) {
      df <- read.csv(file.path(output_folder, "joined_scores_percentile.txt"),
                     sep = "\t")
      write.table(df, file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )
  
  ### DATA ANALYSIS
  
  # Gets uploaded analysis file
  getAnalysisFile <- reactive({
    if(!is.null(input$analysisFile)) { 
      analysis_file_path <- input$analysisFile$datapath 
    }
  })
  
  # Analyzes data on button press
  analyze_data <- eventReactive(input$analyzeDataButton, {
    
    # Builds command to run analyze.R and executes it
    if ((!is.null(analysis_file_path)) &&
        (!is.null(models)) && 
        (!is.null(positive_class))) {
      analyze_output <- NULL
      analyze_running <- TRUE
      analyze_cmd <- paste("Rscript", analyze_file)
      args <- paste("-i", analysis_file_path,
                    "-m", models_folder,
                    "-o", output_folder,
                    "-ex", excluded_models,
                    "-p", positive_class)
      analyze_output <- capture.output(tryCatch(
        system2(analyze_cmd, args), error = function(e) e))
    }
  })
  
  # Sets reactive analysis output text
  output$analysisText <- renderText({
    
    if(analyze_running) { paste("Analyzing data...") }
    
    if(!is.null(analyze_output)) {
      analyze_running <- FALSE
      paste(analyze_output)
    }
  })
  
  # Handler for analyzed data download
  output$downloadAnalyzeButton <- downloadHandler(
    filename = function() { paste("analyzedData.txt") } ,
    content <- function(file) {
      df <- read.csv(file.path(output_folder, "all_predictions.tsv"),
                     sep = "\t")
      write.table(df, file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )
  
  ### MODEL GENERATION 
  
  # Gets uploaded training file
  getAnalysisFile <- reactive({
    if(!is.null(input$trainingFile)) { 
      training_file_path <- input$trainingFile$datapath 
    }
  })
  
  # Generates models on button press
  generate_models <- eventReactive(input$generateModelsButton, {
    
    # Builds command to run generate.R and executes it
    if ((!is.null(training_file_path)) &&
        (!is.null(target_feature))) {
      generate_output <- NULL
      generate_running <- TRUE
      generate_cmd <- paste("Rscript", generate_file)
      args <- paste("-i", training_file_path,
                    "-o", output_folder,
                    "-m", sampling_method,
                    "-r", selection_rule,
                    "-t", target_feature)
      generate_output <- capture.output(tryCatch(
        system2(generate_cmd, args), error = function(e) e))
    }
  })
  
  # Sets reactive generation output text
  output$generateText <- renderText({
    
    if(generate_running) { paste("Generating models...") }
    
    if(!is.null(generate_output)) {
      generate_running <- FALSE
      paste(generate_output)
    }
  })
})

