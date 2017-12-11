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


### DATA ANALYSIS

# Gets uploaded analysis file
getAnalysisFile <- reactive({
  if(!is.null(input$analysisFile)) { 
    analysis_file_path <<- input$analysisFile$datapath 
  }
})

# Analyzes data on button press


observeEvent(input$analyzeDataButton, 
             {
  message_list <- c("Preparing analyze command","Downloading binaries...", "Compiling arguments...", "Mining gold ore",
                    "Merging annotation for gene intervals...", "Creating magic genie...",
                    "Determining status...", "Making coffee...","Running analyze.R")
  withProgress(value = 0, 
               {
                 for (i in 1:length(message_list)){
                   incProgress(1/length(message_list), detail = paste(message_list[i]))
                   Sys.sleep(0.25)
                 }
                 
                 done = FALSE

if (!is.null(output_generate)){
  if(dir.exists(paste(output_generate, "/model_output", sep=""))){
  print("a")
  # Builds command to run analyze.R and executes it
    analyze_output <- NULL
    analyze_running <- TRUE
    analyze_cmd <- paste("Rscript")
    args <- paste(analyze_file,
                  "-i", input$analysisFile$datapath,
                  "-m", paste(output_generate, "/model_output", sep=""),
                  "-o", paste(output_analyze, "/analysis_output", sep=""),
                  "-p", "MAE")
    if(!is.null(input$exModels)) {args <- paste(args, "-ex", paste(unlist(input$exModels), collapse=','))}
    if(!is.null(input$expression_filter)) { args <- paste(args, "-f", input$expressionData) }
    if(!is.null(input$expression_filter)) { args <- paste(args, "-l", input$lengthFilter) }
    print(paste(analyze_cmd, args))
    analyze_output <- capture.output(tryCatch(
    system2(analyze_cmd, args), error = function(e) e))
  }
} else if(!is.null(model_dir)){
    if(dir.exists(paste(model_dir))){
    print("b")
      print(output_analyze)
    # Builds command to run analyze.R and executes it
      analyze_output <- NULL
      analyze_running <- TRUE
      analyze_cmd <- paste("Rscript")
      args <- paste(analyze_file,
                    "-i", input$analysisFile$datapath,
                    "-m", paste(model_dir),
                    "-o", paste(output_analyze, "/analysis_output", sep=""),
                    "-p", "MAE")
      if(!is.null(input$exModels)) {args <- paste(args, "-ex", paste(unlist(input$exModels), collapse=','))}
      if(!is.null(input$expression_filter)) { args <- paste(args, "-f", input$expressionData) }
      if(!is.null(input$expression_filter)) { args <- paste(args, "-l", input$lengthFilter) }
      analyze_output <- capture.output(tryCatch(
        system2(analyze_cmd, args), error = function(e) e))
      cat(args)
    }
} else {
    # Builds command to run analyze.R and executes it
      analyze_output <- NULL
      analyze_running <- TRUE
      analyze_cmd <- paste("Rscript")
      args <- paste(analyze_file,
                    "-i", input$analysisFile$datapath,
                    "-m", models_folder,
                    "-o", paste(output_analyze, "/analysis_output", sep=""),
                    "-p", "MAE")
      if(!is.null(input$exModels)) {args <- paste(args, "-ex", paste(unlist(input$exModels), collapse=','))}
      if(!is.null(input$expression_filter)) { args <- paste(args, "-f", input$expressionData) }
      if(!is.null(input$expression_filter)) { args <- paste(args, "-l", input$lengthFilter) }
      analyze_output <- capture.output(tryCatch(
        system2(analyze_cmd, args), error = function(e) e))
}
                 
})

  
})
# Handler for analyzed data download
# output$downloadAnalyzeButton <- downloadHandler(
#   filename = function() { paste("analyzedData.txt") } ,
#   content <- function(file) {
#     df <- read.csv(paste(output_path, "analysis_output/all_predictions.tsv"),
#                    sep = "\t")
#     write.table(df, file, sep = "\t", row.names = FALSE, quote = FALSE)
#   }
# )


observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$analyzeTableButton
    },
    handlerExpr = {
        predictTable <- load_data(paste(output_analyze, "/analysis_output/all_predictions.tsv", sep=""))
        predictTable %>% dplyr::select(name, grep("_predictions", colnames(predictTable))) -> predictTable
        output$predTbl <- renderDataTable(
          predictTable
        )

    }
    )

