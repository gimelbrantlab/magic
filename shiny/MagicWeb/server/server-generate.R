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
model_dir <<- NULL

# sets output path
shinyDirChoose(input, 'generateOutput', roots = c(home = '~'), filetypes = c('', 'txt','bigWig',"tsv","csv","bw"))
globalGenerateOutput <- reactiveValues(datapath = sub("shiny/MagicWeb", "data/output/", getwd()))
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

# Gets uploaded training file
getAnalysisFile <- reactive({
  if(!is.null(input$trainingFile)) {
    training_file_path <<- input$trainingFile$datapath
  }
})

shinyDirChoose(input, 'modelDir', roots = c(home = '~'), filetypes = c('', 'txt','bigWig',"tsv","csv","bw","rds"))
modelDir <- reactive(input$modelDir)
output$modelDir <- renderPrint(modelDir())

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$modelDir
  },
  handlerExpr = {
    home <- normalizePath("~")
    model_dir <<- file.path(home, paste(unlist(modelDir()$path[-1]), collapse = .Platform$file.sep))
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
    if ((!is.null(input$trainingFile$datapath)) &&
        (!is.null(target_feature))) {
      generate_output <- NULL
      generate_running <- TRUE
      generate_cmd <- paste("Rscript")
      args <- paste(generate_file,
                    "-i", input$trainingFile$datapath,
                    "-o", paste(globalGenerateOutput$datapath, "/model_output", sep=""),
                    "-m", input$metric,
                    "-s", input$samplingMethod,
                    "-r", input$selectionRule,
                    "-t", input$targetFeature,
                    "-l", paste(unlist(input$modelList), collapse=','),
                    "-p", input$trainingPercent,
                    "-c", input$crossValidation
      )
      if(!is.null(input$validationSet$datapath)) {
        args <- paste(args, "-v", input$validationSet$datapath)
      }
      if(!is.null(input$tg2)){
        args <- paste(args, "-a", input$tg2$datapath)
      } else {
        args <- paste(args, "-a", input$tg)
      }
      generate_output <- capture.output(tryCatch(
        system2(generate_cmd, args), error = function(e) e))
      cat(generate_cmd, args)
    }
    modelTable <- load_data(paste(globalGenerateOutput$datapath, "/model_output/summary_models.tsv", sep=""))
    output$modelTbl <- renderDataTable(
      modelTable
    )
    ##### Precision Recall Curve ######
    ##### Get validation set
    if (!is.null(input$validationSet$datapath)) {
      validation <- load_data(input$validationSet$datapath)
      validation[[input$targetFeature]] <- sub(input$positiveClass, "MAE", validation[[input$targetFeature]])
    } else {

      # load one of the training sets in the model output file
      validation_files <- list.files(path = paste(globalGenerateOutput$datapath, "/model_output", sep=""), pattern = "*_testing.tsv")

      # just load the first one
      validation_inner <- load_data(paste(globalGenerateOutput$datapath, "/model_output/", validation_files[1], sep=""))

      # remove the predictions
      validation <- validation_inner[,-4]

      # training_set <- load_data(input$trainingFile$datapath)
      # if (input$tg == "human"){
      #   print("Using human training genes.")
      #   # attach ids
      #   training_set$id <- tolower(paste(training_set$name, training_set$chrom, sep = "_"))
      #   training_set <- training_set[order(training_set$id),]
      #   # load training genes, attach ids
      #   training_genes <- load_data(paste(reference_folder, "/human_tg.tsv", sep=""))
      #   training_genes$id <- tolower(paste(training_genes$gene, training_genes$chrom, sep = "_"))
      #   ids_to_keep <- intersect(training_genes$id, training_set$id)
      #   training_set <- training_set[training_set$id %in% ids_to_keep, ]
      #   training_genes <- training_genes[training_genes$id %in% ids_to_keep, ]
      #   # Appends training genes to modified df
      #   validation <- training_set[order(training_set$id),]
      #   training_genes <- training_genes[order(training_genes$id),]
      #   validation$status <- training_genes$status
      #   validation[["status"]] <- sub(input$positiveClass, "MAE", validation[["status"]])
      # } else if (input$tg == "mouse"){
      #   print("Using mouse training genes.")
      #   # attach ids
      #   training_set$id <- tolower(paste(training_set$name, training_set$chrom, sep = "_"))
      #   training_set <- training_set[order(training_set$id),]
      #   # load training genes, attach ids
      #   training_genes <- load_data(paste(reference_folder, "/mouse_tg.tsv", sep=""))
      #   training_genes$id <- tolower(paste(training_genes$gene, training_genes$chrom, sep = "_"))
      #   ids_to_keep <- intersect(training_genes$id, training_set$id)
      #   training_set <- training_set[training_set$id %in% ids_to_keep, ]
      #   training_genes <- training_genes[training_genes$id %in% ids_to_keep, ]
      #   # Appends training genes to modified df
      #   validation <- training_set[order(training_set$id),]
      #   training_genes <- training_genes[order(training_genes$id),]
      #   validation$status <- training_genes$status
      #   validation[["status"]] <- sub(input$positiveClass, "MAE", validation[["status"]])
      # } else if (!is.null(input$tg2$datapath) & is.null(input$validationSet$datapath)){
      #   print("Using external training dataset.")
      #   # attach ids
      #   training_set$id <- tolower(paste(training_set$name, training_set$chrom, sep = "_"))
      #   training_set <- training_set[order(training_set$id),]
      #   training_genes <- load_data(input$tg2$datapath)
      #   training_genes$id <- tolower(paste(training_genes$gene, training_genes$chrom, sep = "_"))
      #   ids_to_keep <- intersect(training_genes$id, training_set$id)
      #   training_set <- training_set[training_set$id %in% ids_to_keep, ]
      #   training_genes <- training_genes[training_genes$id %in% ids_to_keep, ]
      #   # Appends training genes to modified df
      #   validation <- training_set[order(training_set$id),]
      #   training_genes <- training_genes[order(training_genes$id),]
      #   validation$status <- training_genes$status
      #   validation[[input$targetFeature]] <- sub(input$positiveClass, "MAE", validation[[input$targetFeature]])
      # }
      #
      # cutoff <- createDataPartition(validation$status, p = ((100-input$trainingPercent)/100),
      #                               list = FALSE, times = 1)
      # validation <- validation[-cutoff,]
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

    # my beautiful colors, these are the best colors
    # custom_col <- c("#00a703", "#0002fe", "#000000")


    output$modelPlots <- renderPlot({
      # create plot
      ggplot(aes(x = recall, y = precision, group = model),
             data = results_df_pr) +
        geom_line(aes(color = model), size = 1) +
        #scale_color_manual(values = custom_col) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
              axis.line = element_line(colour = "black"), legend.key=element_blank()) + ylim(0,1)
    })
  })
})

