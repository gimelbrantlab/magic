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
  message_list <- c("Preparing process command","Downloading binaries...", "Compiling arguments...", "Mining gold ore",
                    "Merging annotation for gene intervals...", "Creating magic genie...",
                    "Calculating enrichment...", "Making coffee...", "Moving to America...",
                    "Starting an italian restaurant...", "Having first child...", "Starting postdoc...",
                    "Starting second postdoc...", "Applying for K99...", "Reapplying for K99",
                    "Developing ulcers", "Faculty?","Running process.R")
  withProgress(value = 0, 
               {
                 for (i in 1:length(message_list)){
                   incProgress(1/length(message_list), detail = paste(message_list[i]))
                   Sys.sleep(0.25)
                 }
                 
   done = FALSE
    
  # Builds command to run process.R and executes it
  if (!is.null(input$fileInput)) {
    process_output <- NULL
    process_running <- TRUE
    args <- paste(process_file,
                  "-i", paste(datapath, input$fileInput, sep = ""),
                  "-o", paste(output_path),
                  "-p", input$promoterLength,
                  "-d", input$dropPercent,
                  "-r", input$assembly)
    if(input$cores > 1) { args <- paste(args, "-s", input$cores) }
    if (input$noOverlap == TRUE) { args <- paste(args, "-l") }
    if (!"olfactory genes" %in% input$enableFilters) { args <- paste(args, "-f") }
    if (!"sex chromosomes" %in% input$enableFilters) { args <- paste(args, "-c") }
    if (!"imprinted genes" %in% input$enableFilters) { args <- paste(args, "-m") }
    cat("\n", args, "\n\n")
    process_output <- capture.output(tryCatch(
      system2("Rscript", args))) # error = function(e) e))
    
  }
   done = TRUE
   })
    
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
  
  if (done == TRUE){
    joined_scores_percentile <- load_data(paste(output_path, "joined_scores_percentile.txt", sep = ""))
    joined_scores_norm <- load_data(paste(output_path, "joined_scores_norm.txt", sep = ""))
    
    if ("status" %in% colnames(joined_scores_percentile)){
      output$chipQCnorm <- renderPlot ({
        ggpairs(joined_scores_norm, 
                columns = 3:ncol(joined_scores_percentile)) +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))
      })
      
      output$chipQC <- renderPlot ({
        ggpairs(joined_scores_percentile, 
                columns = 3:ncol(joined_scores_percentile)) +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))
      })
    } else {
      output$chipQCnorm <- renderPlot ({
        ggpairs(joined_scores_norm,
                columns = 3:ncol(joined_scores_percentile)) +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))
      })
      
      output$chipQC <- renderPlot ({
        ggpairs(joined_scores_percentile, 
                columns = 3:ncol(joined_scores_percentile)) +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))
      })
    }
    
      output$norm_table <- renderDataTable(
        joined_scores_norm
      )
      
      output$perc_table <- renderDataTable(
        joined_scores_percentile
      )
  }
  }
)

# Handler for processed data downloa
output$downloadProcessButton <- downloadHandler(
  filename = function() { paste("processedData.txt") } ,
  content <- function(file) {
    df <- read.csv(file.path(output_folder, 
                             "joined_scores_percentile.txt"),
                   sep = "\t")
    write.table(df, file, sep = "\t", row.names = FALSE, quote = FALSE)
  }
)  









