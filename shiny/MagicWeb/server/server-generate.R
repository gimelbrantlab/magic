
#################
# GENERATE SERVER
#################



### MODEL GENERATION 

# Gets uploaded training file
getAnalysisFile <- reactive({
  if(!is.null(input$trainingFile)) { 
    training_file_path <- input$trainingFile$datapath 
  }
})

# Generates models on button press
observeEvent(input$generateModelsButton, {
  
  # Builds command to run generate.R and executes it
  if ((!is.null(training_file_path)) &&
      (!is.null(target_feature))) {
    generate_output <- NULL
    generate_running <- TRUE
    generate_cmd <- paste("Rscript")
    args <- paste(generate_file,
                  "-i", training_file_path,
                  "-o", output_folder,
                  "-m", sampling_method,
                  "-r", selection_rule,
                  "-t", target_feature)
    generate_output <- capture.output(tryCatch(
      system2(generate_cmd, args), error = function(e) e))
  }
})

# Sets reactive generation output text
output$generateText <- renderUI({
  
  line_1 <- ""
  line_2 <- ""
  
  if(generate_running) { line_1 <- "Generating models..." }
  
  if(!is.null(generate_output)) {
    generate_running <- FALSE
    line_2 <- generate_output
  }
  
  HTML(paste(line_1, line_2, sep = "<br/>"))
})

