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

# Gets uploaded training file
getAnalysisFile <- reactive({
  if(!is.null(input$trainingFile)) { 
    training_file_path <<- input$trainingFile$datapath 
  }
})

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$modelDir
  },
  handlerExpr = {
    # prevent launching directoryInput at app start
    if (input$dataDirectory){
      model_dir <<- choose.dir(default = readDirectoryInput(session, 'dataDirectory'))
    }
  }
)

# Generates models on button press
observeEvent(input$generateModelsButton, {
  
  message_list <- c("Preparing generate command","Downloading binaries...", "Compiling arguments...", "Mining gold ore",
                    "Merging annotation for gene intervals...", "Creating magic genie...",
                    "Training models", "Making coffee...", "Moving to America...",
                    "Starting an italian restaurant...", "Having first child...", "Starting postdoc...",
                    "Starting second postdoc...", "Applying for K99...", "Reapplying for K99",
                    "Developing ulcers", "Faculty?","Running generate.R")
  withProgress(value = 0, 
               {
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
                  "-o", paste(output_path, "model_output", sep=""),
                  "-m", input$samplingMethod,
                  "-r", input$selectionRule,
                  "-t", input$targetFeature,
                  "-l", input$modelList,
                  "-c", input$crossValidation
                  )
    if(!is.null(input$validationSet$datapath)) {
      args <- paste(args, "-v", input$validationSet$datapath) 
      }
    if(!is.null(input$tg) & is.null(input$tg2)) 
    { args <- paste(args, "-a", input$tg)
    } else {
        print("You must only choose one training gene file")
      }
    if(!is.null(input$tg2) & is.null(input$tg)) { 
      args <- paste(args, "-a", input$tg2)
    } else {
      print("You must only choose one training gene file")
    }
    print(generate_cmd)
    generate_output <- capture.output(tryCatch(
      system2(generate_cmd, args), error = function(e) e))
  }
})
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

observeEvent(input$plotModelsButton, {
# if (file.exists(paste(output_path, "model_ouput", sep=""))){

  modelTable <- load_data(paste(output_path, "model_output/summary_models.tsv", sep=""))
  output$modelTbl <- renderDataTable(
    modelTable
  )


  # #### Precision Recall Curve ######
  # #### Get validation set
  # validation <- input$validationSet
  # #### get names of models
  # model_names <- get_names(paste(output_path, "model_output", sep=""), pattern = "*_model.rds")
  # # model_list <- list()
  # for (name in model_names){
  #   name_string <- as.name(name)
  #   print(name_string)
  #   model <- readRDS(paste(output_path, "model_output/", name, "_model.rds", sep = ""))
  #   model_list <- c(model_list, name_string = model)
  # }
  # #print(model_list)
  # # apply calc_auprc to testing data
  # model_list_pr <- model_list %>% lapply(calc_auprc, data = validation)
  # model_list_pr %>% lapply(function(the_mod) the_mod$auc.integral)
  # 
  # results_list_pr <- list(NA)
  # index <- 1
  # 
  # # cycle through outputs of pr.curve abd store in a dataframe
  # for(the_pr in model_list_pr){
  #   results_list_pr[[index]] <-
  #     data_frame(recall = the_pr$curve[, 1],
  #                precision = the_pr$curve[, 2],
  #                model = names(model_list_pr)[index])
  #   index <- index + 1
  # }
  # 
  # # bind results into table
  # results_df_pr <- bind_rows(results_list_pr)
  # 
  # # my beautiful colors, these are the best colors
  # # custom_col <- c("#00a703", "#0002fe", "#000000")
  # 
  # 
  # ouput$modelPlots <- renderPlot({
  #   # create plot
  #   ggplot(aes(x = recall, y = precision, group = model),
  #          data = results_df_pr) +
  #     geom_line(aes(color = model), size = 1) +
  #     #scale_color_manual(values = custom_col) +
  #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
  #           axis.line = element_line(colour = "black"), legend.key=element_blank()) + ylim(0,1)
  # 
  # })


})
