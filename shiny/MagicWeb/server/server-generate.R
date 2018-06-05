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

#################
# GENERATE SERVER
#################

###############
# USE EXISTING MODEL
###############

# framework held in server-analyze.R

###############
# GENERATE NEW MODEL
###############

# sets output path
shinyDirChoose(input, 'generateOutput', roots = c(home = '~'), filetypes = c('', 'txt','bigWig',"tsv","csv","bw"))
globalGenerateOutput <- reactiveValues(datapath = getwd())
generateOutput <- reactive(input$generateOutput)
output$generateOutput <- renderText({ globalGenerateOutput$datapath })

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$generateOutput
  },
  handlerExpr = {
    home <- normalizePath("~")
    globalGenerateOutput$datapath <- file.path(home, paste(unlist(generateOutput()$path[-1]), collapse = .Platform$file.sep))
  }
)

# Uploads training file
shinyFileChoose(input, 'trainingFile', roots = c(home = '~'))
globalTrainingFile <- reactiveValues(datapath = paste0(getwd(), "/data/joined_scores_percentile_GM12878.txt"))
output$trainingFileText <- renderText({ globalTrainingFile$datapath })

observeEvent(input$trainingFile, {
  inFile <- parseFilePaths(roots=c(home = '~'), input$trainingFile)
  globalTrainingFile$datapath <- as.character(inFile$datapath)
}
)

# Uploads optional training genes file
shinyFileChoose(input, 'tg2', roots = c(home = '~'))
globalTg2 <- reactiveValues(datapath = NULL)
output$tg2Text <- renderText({ globalTg2$datapath })

observeEvent(input$tg2, {
  inFileTg2 <- parseFilePaths(roots=c(home = '~'), input$tg2)
  globalTg2$datapath <- as.character(inFileTg2$datapath)
}
)

# Uploads optional validation set file
shinyFileChoose(input, 'validationSet', roots = c(home = '~'))
globalValidationSet <- reactiveValues(datapath = NULL)
output$validationSetText <- renderText({ globalValidationSet$datapath })

observeEvent(input$validationSet, {
  inFileVal <- parseFilePaths(roots=c(home = '~'), input$validationSet)
  globalValidationSet$datapath <- as.character(inFileVal$datapath)
}
)


# Generates models on button press
observeEvent(input$generateModelsButton, {
  
  message_list <- c("Preparing generate command","Loading files", "Running generate.R")
  
  withProgress(value = 0, {
    for (i in 1:length(message_list)){
      incProgress(1/length(message_list), detail = paste(message_list[i]))
      Sys.sleep(0.25)
    }
    # Builds command to run generate.R and executes it
    if (is.null(input$modelList)) {
      showModal(modalDialog(
        title = "Error",
        "Please select at least one model to train",
        easyClose = TRUE
      ))
    }
    else {
      if ((is.null(globalTrainingFile$datapath)) | (is.null(target_feature))) {
        showModal(modalDialog(
          title = "Error",
          "Please select input file and target feature and rerun",
          easyClose = TRUE
        ))
      }
      else {
        if ((globalTrainingFile$datapath==paste0(getwd(), "/data/joined_scores_percentile_GM12878.txt"))&(input$tg !="human")) {
          showModal(modalDialog(
            title = "Error",
            "Input file with processed chromatine and file with training genes are inconsistent, did you mean to select human?",
            easyClose = TRUE
          ))
        }
        else {
          if ((is.null(globalValidationSet$datapath))&(input$trainingPercent == 100)) {
            showModal(modalDialog(
              title = "Warning",
              "Using 100% of training data for training and no validation file is specified, no summary file will be created",
              easyClose = TRUE
            ))
          }
          summ_file <- paste(globalGenerateOutput$datapath, "/model_output/summary_models.tsv", sep="")
          if (file.exists(summ_file)) {
            file.remove(summ_file)
          }
          generate_output <- NULL
          generate_running <- TRUE
          generate_cmd <- paste("Rscript")
          args <- paste(generate_file,
                        "-i", globalTrainingFile$datapath,
                        "-o", paste(globalGenerateOutput$datapath, "/model_output", sep=""),
                        "-m", input$metric,
                        "-s", input$samplingMethod,
                        "-r", input$selectionRule,
                        "-t", input$targetFeature,
                        "-l", paste(unlist(input$modelList), collapse=','),
                        "-p", input$trainingPercent,
                        "-c", input$crossValidation
          )
          if(!is.null(globalValidationSet$datapath)) {
            args <- paste(args, "-v", globalValidationSet$datapath)
          }
          if(!is.null(globalTg2$datapath)){
            args <- paste(args, "-a", globalTg2$datapath)
          } else {
            args <- paste(args, "-a", input$tg)
          }
          generate_output <- capture.output(tryCatch(
            system2(generate_cmd, args), error = function(e) e))
          cat(generate_cmd, args, "\n")
          # Checking if file is there
          summ_file <- paste(globalGenerateOutput$datapath, "/model_output/summary_models.tsv", sep="")
          if (!file.exists(summ_file)) {
            showModal(modalDialog(
              title = "Warning",
              "No summary file was created, please see terminal output for specifics",
              easyClose = TRUE
            ))
            output$modelPlots <- NULL
            output$modelTbl <- NULL
          }
          else {
            modelTable <- load_data(paste(globalGenerateOutput$datapath, "/model_output/summary_models.tsv", sep=""))
            output$modelTbl <- renderDataTable(
              modelTable
            )
            ##### Precision Recall Curve ######
            ##### Get validation set
            if (!is.null(globalValidationSet$datapath)) {
              validation <- load_data(globalValidationSet$datapath)
              validation[[input$targetFeature]] <- sub(input$positiveClass, "MAE", validation[[input$targetFeature]])
            } else {
              
              # load one of the training sets in the model output file
              validation_files <- list.files(path = paste(globalGenerateOutput$datapath, "/model_output", sep=""), pattern = "*_testing.tsv")
              
              # just load the first one
              validation_inner <- load_data(paste(globalGenerateOutput$datapath, "/model_output/", validation_files[1], sep=""))
              
              # remove the prediction
              validation <- validation_inner %>% dplyr::select(-(prediction))
            }
            
            #### get names of models
            model_names <- list.files(paste(globalGenerateOutput$datapath, "/model_output", sep=""), pattern = "*_model.rds")
            model_list <- list()
            # assign models to r environment variables
            for (i in 1:length(model_names)){
              name_string <- as.character(model_names[i])
              assign(name_string, readRDS(paste(globalGenerateOutput$datapath, "/model_output/", model_names[i], sep = "")))
            }
            # make a list of models
            model_list <- list()
            for (i in 1:length(model_names)){
              model_list <- c(model_list, mget(as.character(model_names[i])))
            }
            
            # apply calc_auprc to testing data
            model_list_pr <- model_list %>% lapply(calc_auprc, data = validation, status = input$targetFeature)
            model_list_pr %>% lapply(function(the_mod) the_mod$auc.integral)
            
            results_list_pr <- list(NA)
            index <- 1
            
            # cycle through outputs of pr.curve abd store in a dataframe
            for(the_pr in model_list_pr){
              results_list_pr[[index]] <-
                data_frame(recall = the_pr$curve[, 1],
                           precision = the_pr$curve[, 2],
                           model = names(model_list_pr)[index])
              index <- index + 1
            }
            
            # bind results into table
            results_df_pr <- bind_rows(results_list_pr)
            
            output$modelPlots <- renderPlot({
              # create plot
              ggplot(aes(x = recall, y = precision, group = model),
                     data = results_df_pr) +
                geom_line(aes(color = model), size = 1) +
                #scale_color_manual(values = custom_col) +
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
                      axis.line = element_line(colour = "black"), legend.key=element_blank()) + ylim(0,1)
            })
          }
        }
      }
    }
  })
})

