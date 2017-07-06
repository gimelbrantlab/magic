
################
# ANALYZE SERVER
################



### DATA ANALYSIS

# Gets uploaded analysis file
getAnalysisFile <- reactive({
  if(!is.null(input$analysisFile)) { 
    analysis_file_path <- input$analysisFile$datapath 
  }
})

# Analyzes data on button press
observeEvent(input$analyzeDataButton, {
  
  # Builds command to run analyze.R and executes it
  if ((!is.null(analysis_file_path)) &&
      (!is.null(models)) && 
      (!is.null(positive_class))) {
    analyze_output <- NULL
    analyze_running <- TRUE
    analyze_cmd <- paste("Rscript")
    args <- paste(analyze_file,
                  "-i", analysis_file_path,
                  "-m", models_folder,
                  "-o", output_folder,
                  "-ex", excluded_models,
                  "-p", positive_class)
    analyze_output <- capture.output(tryCatch(
      system2(analyze_cmd, args), error = function(e) e))
  }
})

# Sets reactive analysis output text
output$analysisText <- renderUI({
  
  line_1 <- ""
  line_2 <- ""
  
  if(analyze_running) { line_1 <- "Analyzing data..." }
  
  if(!is.null(analyze_output)) {
    analyze_running <- FALSE
    line_2 <- analyze_output
  }
  
  HTML(paste(line_1, line_2, sep = "<br/>"))
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