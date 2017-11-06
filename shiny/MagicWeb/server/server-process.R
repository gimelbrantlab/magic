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
  done = FALSE
  withProgress(value = 0, 
               {
                 for (i in 1:length(message_list)){
                   incProgress(1/length(message_list), detail = paste(message_list[i]))
                   Sys.sleep(0.25)
                   done = TRUE
                 }
                 
  
    
  # Builds command to run process.R and executes it
  if (!is.null(input$fileInput)) {
    cat(output_path)
    process_output <- NULL
    process_running <- TRUE
    args <- paste(process_file,
                  "-i", paste(datapath, "/", input$fileInput, sep = ""),
                  "-o", paste(output_path),
                  "-p", input$promoterLength,
                  "-d", input$dropPercent,
                  "-r", input$assembly,
                  "-e")
    if(input$cores > 1) { args <- paste(args, "-s", input$cores) }
    if (input$noOverlap == TRUE) { args <- paste(args, "-l") }
    if (!"olfactory genes" %in% input$enableFilters) { args <- paste(args, "-f") }
    if (!"sex chromosomes" %in% input$enableFilters) { args <- paste(args, "-c") }
    if (!"imprinted genes" %in% input$enableFilters) { args <- paste(args, "-m") }
    cat("\n", args, "\n\n")
    process_output <- capture.output(tryCatch(
      system2("Rscript", args))) # error = function(e) e))
    
  }
   
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
  
  if (file.exists(paste(output_path, "/joined_scores_percentile.txt", sep = ""))){
    joined_scores_percentile <- load_data(paste(output_path, "/joined_scores_percentile.txt", sep = ""))
    joined_scores_norm <- load_data(paste(output_path, "/joined_scores_norm.txt", sep = ""))
    
    
      output$chipQCnorm <- renderPlot ({
        ggpairs(joined_scores_norm,
                columns = 3:ncol(joined_scores_percentile),
                upper = "blank",
                diag = NULL) +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))
      })
      
      output$chipQC <- renderPlot ({
        ggpairs(joined_scores_percentile, 
                columns = 3:ncol(joined_scores_percentile),
                upper = "blank",
                diag = NULL) +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))
      })
      
      output$perc_table <- renderDataTable(
        joined_scores_percentile
      )
      
      # if(input$promoterLength > 0){
      #   inputFiles <- list.files()
      # } else{
      #   inputFiles <- lapply(Sys.glob("*input*.txt"), read.csv)
      # }
      input_files <- list.files(output_path, pattern = "_input", full.names = TRUE)
      input_scores <- lapply(input_files, load_data)
      detach("package:MASS", unload=TRUE)
      plotList <- list()
      for (i in 1:length(input_scores)){
        df <- data.frame(input_scores[i])
        df %>% select(mean) -> df_culled
        df_small <- reshape2::melt(df_culled)
        p <- ggplot(df_small, aes(x=variable, y=value)) + geom_violin() + ylim(0,2) + xlab("Input") + ggtitle(paste(input_files[i]))
        assign(paste("plot",i,sep=""), p)
        plotList[[i]] <- p
      }
      output$inputDist <- renderPlot({
        ggmatrix(plotList, nrow=2, ncol=length(plotList), 
                 yAxisLabels = "Mean input", 
                 xAxisLabels = input_files) +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                axis.line = element_line(colour = "black"))
      })
  }
  }
)


