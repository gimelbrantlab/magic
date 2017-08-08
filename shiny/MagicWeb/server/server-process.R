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
# PROCESS SERVER
################

### FOR GLOBAL VARIABLES REFERENCE SERVER.R

### VARIABLES AND DATA


### DATA PROCESSING
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
# Processes data on button press
observeEvent(input$processDataButton, 
  {
  withProgress(message = "Running process.R", value = 0, {
   done = FALSE
    
    
  # Builds command to run process.R and executes it
  if (!is.null(input$fileInput)) {
    process_output <- NULL
    process_running <- TRUE
    args <- paste(process_file,
                  "-i", paste(datapath, input$fileInput, sep = ""),
                  "-p", input$promoterLength,
                  "-d", input$dropPercent,
                  "-r", "hg19")
    if(input$cores > 1) { args <- paste(args, "-s", input$cores) }
    # if() { args <- paste(args, "-f") }
    # if (no_overlap()) { args <- paste(args, "-l") }
    cat("\n", args, "\n\n")
    process_output <- capture.output(tryCatch(
      system2("Rscript", args))) # error = function(e) e))
    
    message_list <- c("Downloading binaries...", "Compiling arguments...", "Mining gold ore",
                      "Merging annotation for gene intervals...", "Creating magic genie...",
                      "Calculating enrichment...", "Making coffee...", "Moving to America...",
                      "Starting an italian restaurant...", "Having first child...", "Starting postdoc...",
                      "Starting second postdoc...", "Applying for K99...", "Reapplying for K99")
    
    for(i in 1:length(message_list)){
      incProgress(1/length(message_list), detail = paste(message_list[i]))
    }
  }
  }
  )
    
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################

  ########################## Data plotting #############################
  
  joined_scores_percentile <- load_data("./output/joined_scores_percentile")
  joined_scores_norm <- load_data("./output/joined_scores_norm.txt")
  
  # plot genomic distribution
  if (input$tg != "none"){
  output$trainingDist <- renderPlot ({
    genomic_distribution(mouse_obs)
  })
  }
  }
)

# Handler for processed data download
output$downloadProcessButton <- downloadHandler(
  filename = function() { paste("processedData.txt") } ,
  content <- function(file) {
    df <- read.csv(file.path(output_folder, 
                             "joined_scores_percentile.txt"),
                   sep = "\t")
    write.table(df, file, sep = "\t", row.names = FALSE, quote = FALSE)
  }
)  




