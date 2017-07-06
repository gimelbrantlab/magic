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

### DATA PROCESSING

# Processes data on button press
observeEvent(input$processDataButton, {
  
  # Gets processing directory from widget
  process_dir <- readDirectoryInput(session, 'file_input')
  
  # Builds command to run process.R and executes it
  if (!is.null(process_dir)) {
    process_output <- NULL
    process_running <- TRUE
    process_cmd <- paste("Rscript")
    args <- paste(process_file,
                  "-i", process_dir,
                  "-o", output_folder,
                  "-p", promoter_length(),
                  "-d", drop_percent(),
                  "-r", refseq_name(),
                  "-t", training_genes())
    if(cores()) { args <- paste(args, "-s") }
    if(no_filter_olf()) { args <- paste(args, "-f") }
    if (disable_filtering()) { args <- paste(args, "-f") }
    if (no_overlap()) { args <- paste(args, "-l") }
    cat("\n", args, "\n\n")
    process_output <- capture.output(tryCatch(
      system2(process_cmd, args), error = function(e) e))
  }
})


# Sets reactive processing output text
output$processText <- renderUI({
  
  line_1 <- "Processing text goes here"
  line_2 <- ""
  line_3 <- ""
  
  if(process_running) { line_2 <- "Processing data..." }
  
  if(!is.null(process_output)) {
    process_running <- FALSE
    line_3 <- process_output
  }
  
  HTML(paste(line_1, line_2, line_3, sep = "<br/>"))
})

# Handler for processed data download
output$downloadProcessButton <- downloadHandler(
  filename = function() { paste("processedData.txt") } ,
  content <- function(file) {
    df <- read.csv(file.path(output_folder, "joined_scores_percentile.txt"),
                   sep = "\t")
    write.table(df, file, sep = "\t", row.names = FALSE, quote = FALSE)
  }
)

# Output data table
output$processTable <- 
  
  # Output plots about data
  output$processPlots <- renderPlot({
    
    ggplot(outpu)
    
    
  })