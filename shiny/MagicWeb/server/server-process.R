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
                    "Calculating enrichment...", "Making coffee...", "Running process.R")
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
    if (!"olfactory receptor genes" %in% input$enableFilters) { args <- paste(args, "-f") }
    if (!"sex chromosomes" %in% input$enableFilters) { args <- paste(args, "-c") }
    if (!"imprinted genes" %in% input$enableFilters) { args <- paste(args, "-m") }
    cat("\n", args, "\n\n")
    process_output <- capture.output(tryCatch(
      { system2("Rscript", args)}, error=function(err){
        traceback()
      })
      ) 
    
  }
   
   })
    
  ###################################################################################################
  ###################################################################################################

  ########################## Data plotting #############################
  
  if (file.exists(paste(output_path, "/joined_scores_percentile.txt", sep = ""))){
    joined_scores_percentile <- load_data(paste(output_path, "/joined_scores_percentile.txt", sep = ""))
    joined_scores_norm <- load_data(paste(output_path, "/joined_scores_norm.txt", sep = ""))
    
    
      output$chipQC_density <- renderPlot ({
        ggplot(data = melt(joined_scores_norm), mapping = aes(x = value)) + geom_density() + scale_x_log10() + theme_bw() + facet_wrap(~variable, scales = 'free_x')
      })
      
      output$chipQC <- renderPlot ({
        marks <- colnames(joined_scores_percentile[,3:dim(joined_scores_percentile)[2]])
        marks_comb <- combn(marks, 2)
        makePlot <- function(x) {
          ggplot(joined_scores_percentile, aes_string(x=x[1], y=x[2])) + geom_point(size=0.3) + theme_bw() 
        }
        pltList <- list()
        pltList <- apply(marks_comb,2,makePlot)
        grid.arrange(grobs = pltList)
      })
      
      output$perc_table <- renderDataTable(
        joined_scores_percentile
      )
      
      
      output$inputDist <- renderPlot({
      if(input$promoterLength > 0){
        inputFiles <- list.files()
      } else{
        inputFiles <- lapply(Sys.glob("*input*.txt"), read.csv)
      }
      input_files <- list.files(output_path, pattern = "_input", full.names = TRUE)
      input_scores <- lapply(input_files, load_data)
      input_scores <- unique(input_scores)
      plotList <- list()
      makePlotInput <- function(x) {
        df <- data.frame(x)
        p <- ggplot(df, aes(mean)) + 
          geom_density() + 
          xlab("Input") + 
          scale_x_log10() +
          theme_bw()
        p
      }
      plotList <- lapply(input_scores, makePlotInput)
      grid.arrange(grobs = plotList)
    })
  }
  }
)
