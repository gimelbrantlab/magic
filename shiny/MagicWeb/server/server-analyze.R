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

expression_file_path <- NULL

# sets output path
shinyDirChoose(input, 'analyzeOutput', roots = c(home = '~'), filetypes = c('', 'txt','bigWig',"tsv","csv","bw"))
globalAnalyzeOutput <- reactiveValues(datapath = getwd())
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
globalModelInput <- reactiveValues(datapath = paste0(getwd(), "/models/"))
modelFolder <- reactive(input$modelFolder)
output$modelFolder <- renderText({ globalModelInput$datapath })

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$modelFolder
  },
  handlerExpr = {
    home <- normalizePath("~")
    globalModelInput$datapath <- file.path(home, paste(unlist(modelFolder()$path[-1]), collapse = .Platform$file.sep))
  }
)


### DATA ANALYSIS

# Uploads analysis file
shinyFileChoose(input, 'analysisFile', roots = c(home = '~'))
globalAnalysisFile <- reactiveValues(datapath = paste0(getwd(), "/data/joined_scores_percentile_GM12878.txt"))
output$analysisFileText <- renderText({ globalAnalysisFile$datapath })

observeEvent(input$analysisFile, {
  inFileAn <- parseFilePaths(roots=c(home = '~'), input$analysisFile)
  globalAnalysisFile$datapath <- as.character(inFileAn$datapath)
}
)

# Uploads optional file with length
shinyFileChoose(input, 'expressionData', roots = c(home = '~'))
globalExpressionData <- reactiveValues(datapath = NULL)
output$expressionDataText <- renderText({ globalExpressionData$datapath })

observeEvent(input$expressionData, {
  inFileEx <- parseFilePaths(roots=c(home = '~'), input$expressionData)
  globalExpressionData$datapath <- as.character(inFileEx$datapath)
}
)

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
        if (input$exprFilt) {
          if (input$filterFile == 'custom') {
            if (!is.null(input$expressionData)) {
              expression_file_path <- globalExpressionData$datapath
            }
            else {
              showModal(modalDialog(
                title = "Error",
                "Please, upload file with gene lengths, or select human or mouse",
                easyClose = TRUE
              ))
            }
          }
          else {
            if (input$filterFile == 'human (expression in GM12878)') {
              expression_file_path <- file.path(reference_folder, "hg19_expr_GM12878.txt")
            }
            if (input$filterFile == 'mouse') {
              expression_file_path <- file.path(reference_folder, "mm10_length.txt")
            }
          }
        }
        # Builds command to run analyze.R and executes it
        analyze_output <- NULL
        analyze_running <- TRUE
        analyze_cmd <- paste("Rscript")
        args <- paste(analyze_file,
                      "-i", globalAnalysisFile$datapath,
                      "-m", ifelse(input$bestMod, paste0(globalModelInput$datapath, "best_model/ "), paste(globalModelInput$datapath)),
                      "-o", paste(globalAnalyzeOutput$datapath, "/analysis_output", sep=""),
                      "-p", "MAE")
        if ((input$exprFilt)&(!is.null(expression_file_path))) { args <- paste(args, "-r", "-f", expression_file_path) }
        if ((input$lengthFilt)&(!is.null(input$lengthFilter))) { args <- paste(args, "-l", input$lengthFilter, "-s", input$filterOrg) }
        if (!input$lengthFilt) { args <- paste(args, "-l", 0) }
        cat(args, "\n")
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
        # Plotting
        joined_scores_percentile <- load_data(globalAnalysisFile$datapath)
        output$analyzePlots <- renderPlot({
          file_with_pr <- paste0(globalAnalyzeOutput$datapath, "/analysis_output/", input$modelToPlot, "_predictions.tsv")
          if (file.exists(file_with_pr)) {
            predd <- read.delim(file_with_pr)
            marks <- colnames(joined_scores_percentile[,3:dim(joined_scores_percentile)[2]])
            jj <- merge(joined_scores_percentile, predd[,c("name", "predictions")], by.x="name", by.y="name")
            marks_comb <- combn(marks, 2)
            makePlot <- function(x) {
              ggplot(jj, aes_string(x=x[1], y=x[2], col=factor(jj$predictions))) + 
                geom_point(size=0.3) + 
                scale_color_manual(values=c("#FC6621", "#105CFB"), name ="group") +
                theme_bw()
            }
            pltList <- list()
            pltList <- apply(marks_comb,2,makePlot)
            grid.arrange(grobs = pltList)  
          }
          else {
            showModal(modalDialog(
              title = "Error",
              "Please select existing model",
              easyClose = TRUE
            ))
          }
        })
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

