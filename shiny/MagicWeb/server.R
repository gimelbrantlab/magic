
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
      process_cmd <- paste("Rscript", process_dir)
      args <- paste("-i", process_dir,
                    "-o", output_folder,
                    "-f", disable_filtering,
                    "-p", promoter_length,
                    "-d", drop_percent,
                    "-l", no_overlap,
                    "-r", refseq_name,
                    "-tr", training_genes)
      try(system2(process_cmd, args))
    }
  })
  
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
      analyze_cmd <- paste("Rscript", analyze_file)
      args <- paste("-i", analysis_file_path,
                    "-m", models_folder,
                    "-o", output_folder,
                    "-ex", excluded_models,
                    "-po", positive_class)
      try(system2(analyze_cmd, args))
    }
  })
  
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
      generate_cmd <- paste("Rscript", generate_file)
      args <- paste("-i", training_file_path,
                    "-o", output_folder,
                    "-sa", sampling_method,
                    "-se", selection_rule,
                    "-ta", target_feature)
      try(system2(generate_cmd, args))
    }
  })
})

