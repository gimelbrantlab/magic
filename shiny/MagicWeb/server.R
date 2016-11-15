
######
### SERVER LIBRARIES AND SCRIPTS
######


library(shiny)


######
### SERVER GLOBALS
######


src_folder <- file.path("..", "..", "src")
process_file <- file.path(src_folder, "process.R")
generate_file <- file.path(src_folder, "generate.R")
analyze_file <- file.path(src_folder, "analyze.R")

######
### SERVER
######


shinyServer(function(input, output, session) {
  
  # Session variables
  process_dir <- NULL
  
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
})

