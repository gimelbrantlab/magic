
######
### SERVER LIBRARIES AND SCRIPTS
######


library(shiny)
source(file.path("..", "..", "src", "process.R"))
source(file.path("..", "..", "src", "generate.R"))
source(file.path("..", "..", "src", "analyze.R"))

######
### SERVER GLOBALS
######


######
### SERVER
######


shinyServer(function(input, output, session) {
  
  # Session variables
  process_dir <- NULL
  cat(file=stderr(), "process_dir")
  
  ### DATA PROCESSING 
  
  # Gets selected input folder
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$processingDirButton
    },
    handlerExpr = {
      if (input$processingDirButton > 0) {
        print("getting dir...")
        process_dir = 
          choose.dir(default = readDirectoryInput(session, 'processingDirButton'))
        updateDirectoryInput(session, 'processingDirButton', value = process_dir)
      }
    }
  )
})

