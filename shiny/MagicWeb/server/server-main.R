#######
# MAIN SERVER
#######


# Gets selected input folder
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$processingDir
  },
  handlerExpr = {
    if (input$processingDir > 0) {
      temp_dir <- choose.dir(default = readDirectoryInput(session, 'processingDir'))
      updateDirectoryInput(session, 'processingDir', value = temp_dir)
    }
  }
)

# action buttons between tabs
observeEvent(
  input$get_started,
  {updateTabsetPanel(session = session, inputId = "main_panel", selected = "input")}
)

observeEvent(
  input$next_process,
  {updateTabsetPanel(session = session, inputId = "main_panel", selected = "process")}
)

observeEvent(
  input$next_generate,
  {updateTabsetPanel(session = session, inputId = "main_panel", selected = "generate")}
)

observeEvent(
  input$next_analyze,
  {updateTabsetPanel(session = session, inputId = "main_panel", selected = "analyze")}
)