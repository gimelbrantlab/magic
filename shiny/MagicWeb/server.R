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


######
### SERVER LIBRARIES AND SCRIPTS
######


######
### SERVER GLOBALS
######

process_file <- file.path(src_folder, "process.R")
generate_file <- file.path(src_folder, "generate.R")
analyze_file <- file.path(src_folder, "analyze.R")
model_names <- get_names(models_folder, pattern = "*_model.rds")

######
### SERVER
######

shinyServer(function(input, output, session) {
  
  # Session variables
  temp_dir <- NULL
  no_overlap <- NULL
  
  # new process arguments
  process_dir <- NULL
  no_filter_olf <- NULL
  no_filter_chrom <- NULL
  no_filter_imprinted <- NULL
  promoter_length <- NULL
  refseq_name <- NULL
  drop_percent <- NULL
  cores <- NULL
  training_genes <- NULL
  
  # new generate arguments
  model_list <- NULL
  positive_class <- NULL
  excluded_models <- NULL
  target_feature <- NULL
  sampling_method <- NULL
  selection_rule <- NULL
  
  # analysis arguments
  analysis_file_path <- NULL
  
  # Session variables for program output
  process_output <- NULL
  process_running <- FALSE
  analyze_output <- NULL
  analyze_running <- FALSE
  generate_output <- NULL
  generate_running <- FALSE
  
  ### VARIABLE UPDATING
  
  positive_class <- reactive({ input$positiveClass })
  target_feature <- reactive({ input$targetFeature })
  sampling_method <- reactive({ input$samplingMethod })
  selection_rule <- reactive({ input$selectionRule })
  excluded_models <- reactive({ paste(model_names[!(model_names %in% input$models)],
                                      sep = ",", collapse = "") })
  
  
  shinyDirChoose(input, 'dir', roots = c(home = '~'), filetypes = c('', 'txt','bigWig',"tsv","csv","bw"))
  dir <- reactive(input$dir)
  #output$dir <- renderPrint(dir())
  output$dir <- renderText({parseDirPath(c(home = '~'), dir())})
  
  # path
  # datapath <<- reactive({
  #   home <- normalizePath("~")
  #   file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
  # })
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$dir
    },
    handlerExpr = {
      home <- normalizePath("~")
      datapath <<- file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
    }
  )
  
  shinyDirChoose(input, 'outputPath', roots = c(home = '~'), filetypes = c('', 'txt','bigWig',"tsv","csv","bw","rds"))
  outputPath <- reactive(input$outputPath)
  #output$outputPath <- renderPrint(outputPath())
  output$outputPath <- renderText({parseDirPath(c(home = '~'), outputPath())})
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$outputPath
    },
    handlerExpr = {
      home <- normalizePath("~")
      output_path <<- file.path(home, paste(unlist(outputPath()$path[-1]), collapse = .Platform$file.sep))
    }
  )
  
  
  
  source("server/server-main.R", local=TRUE)$value
  source("server/server-analyze.R", local=TRUE)$value
  source("server/server-generate.R", local=TRUE)$value
  source("server/server-process.R", local=TRUE)$value
})

