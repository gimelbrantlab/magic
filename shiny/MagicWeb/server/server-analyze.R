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
shinyDirChoose(input, 'analyzeOutput', roots = c(home = '~'), filetypes = c('', 'txt','bigWig',"tsv","csv","bw"))
globalAnalyzeOutput <- reactiveValues(datapath = sub("shiny/MagicWeb", "data/output/", getwd()))
analyzeOutput <- reactive(input$analyzeOutput)
output$analyzeOutput <- renderText({ globalAnalyzeOutput$datapath })

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$analyzeOutput
  },
  handlerExpr = {
    home <- normalizePath("~")
    globalAnalyzeOutput$datapath <- file.path(home, paste(unlist(analyzeOutput()$path[-1]), collapse = .Platform$file.sep))
  }
)

# sets models folder path
shinyDirChoose(input, 'modelFolder', roots = c(home = '~'), filetypes = c('', 'txt','rds'))
globalModelInput <- reactiveValues(datapath = sub("shiny/MagicWeb", "models", getwd()))
modelFolder <- reactive(input$modelFolder)
output$modelFolder <- renderText({ globalModelInput$datapath })

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$modelFolder
  },
  handlerExpr = {
    home <- normalizePath("~")
    globalModelInput$datapath <- file.path(home, paste(unlist(analyzeOutput()$path[-1]), collapse = .Platform$file.sep))
  }
)


### DATA ANALYSIS

# Gets uploaded analysis file
getAnalysisFile <- reactive({
  if(!is.null(input$analysisFile)) {
    analysis_file_path <<- input$analysisFile$datapath
  }
})

# Analyzes data on button press
observeEvent(input$analyzeDataButton, {
  message_list <- c("Preparing analyze command","Loading files","Running analyze.R")
  withProgress(value = 0, {
    for (i in 1:length(message_list)){
      incProgress(1/length(message_list), detail = paste(message_list[i]))
      Sys.sleep(0.25)
    }
    if (dir.exists(paste(globalModelInput$datapath))) {
      filenames <- Sys.glob(file.path(globalModelInput$datapath, "*_model.rds"))
      if (length(filenames)>0) {
        # get filter file
        if (!is.null(input$expressionData)) {
          if (input$filterFile == 'custom') {
            expression_file_path <- input$expressionData$datapath
          }
          else {
            if (input$filterFile == 'human') {
              expression_file_path <- file.path(reference_folder, "hg19_length.txt")
            }
            if (input$filterFile == 'mouse') {
              expression_file_path <- file.path(reference_folder, "mm10_length.txt")
            }
          }
        }
        else {
          if (input$filterFile == 'human') {
            expression_file_path <- file.path(reference_folder, "hg19_length.txt")
          }
          if (input$filterFile == 'mouse') {
            expression_file_path <- file.path(reference_folder, "mm10_length.txt")
          }
          if (input$filterFile == 'custom') {
            showModal(modalDialog(
              title = "Error",
              "Please, upload file with gene lengths, or select human or mouse",
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
                      "-m", paste(globalModelInput$datapath),
                      "-o", paste(globalAnalyzeOutput$datapath, "/analysis_output", sep=""),
                      "-p", "MAE")
        if ((input$lengthFilt)&(!is.null(input$lengthFilter))) { args <- paste(args, "-f", expression_file_path, "-l", input$lengthFilter) }
        #if(!is.null(input$lengthFilt)) { args <- paste(args, "-l", input$lengthFilter) }
        cat(args)
        analyze_output <- capture.output(tryCatch(
          system2(analyze_cmd, args), error = function(e) e))
        sets_files <- list.files(paste(globalAnalyzeOutput$datapath, "/analysis_output", sep=""), pattern = "*_predictions.tsv", recursive = FALSE)
        i <- 0
        for (file in sets_files) {
          p_set <- read.csv(file.path(paste(globalAnalyzeOutput$datapath, "/analysis_output", sep=""), file), sep = "\t")
          pr_name <- gsub('.{4}$', '', file)
          colnames(p_set)[5] = pr_name
          if (i == 0) {
            df <- p_set[,c(1,5)]
          }
          else {
            df <- merge(df, p_set[,c(1,5)], by.x="name", "name")
          }
          i <- i+1
        }
        output$predTbl <- renderDataTable(
          df
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

