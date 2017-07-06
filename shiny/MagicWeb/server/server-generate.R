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



### MODEL GENERATION 

# Gets uploaded training file
getAnalysisFile <- reactive({
  if(!is.null(input$trainingFile)) { 
    training_file_path <- input$trainingFile$datapath 
  }
})

# Generates models on button press
observeEvent(input$generateModelsButton, {
  
  # Builds command to run generate.R and executes it
  if ((!is.null(training_file_path)) &&
      (!is.null(target_feature))) {
    generate_output <- NULL
    generate_running <- TRUE
    generate_cmd <- paste("Rscript")
    args <- paste(generate_file,
                  "-i", training_file_path,
                  "-o", output_folder,
                  "-m", sampling_method,
                  "-r", selection_rule,
                  "-t", target_feature)
    generate_output <- capture.output(tryCatch(
      system2(generate_cmd, args), error = function(e) e))
  }
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

