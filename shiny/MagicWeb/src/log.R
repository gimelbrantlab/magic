# Backend of the activity log tab in the Shiny app

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
# MAIN FUNCTIONS
######

# Writes a string, formatted as a new entry, to the given filepath
write_log <- function(log_file, entry) {
  current_date <- format(Sys.Date(), format="%B %d, %Y")
  date_header <- paste("###", current_date)
  create_header <- paste("Activity log created on", current_date)
  update_header <- paste("Activity log last updated on", current_date,
                         "at", format(Sys.time(), "%X"))
  newly_created <- FALSE
  
  # Opens connection to log and creates new log if necessary
  lines <- NA
  conn <- file(log_file)
  if (!file.exists(log_file)) {  
    newly_created <- TRUE
    writeLines(c("", create_header, update_header), conn)
    lines <- readLines(conn)
  } else {
    lines <- readLines(conn)
  }
  close(conn)
  
  # Checks to see if there is an entry for the current day
  # and records that line number if it exists. Otherwise,
  # the current date index is set to the first line
  current_date_line <- match(date_header, lines)
  if (is.na(current_date_line)) { current_date_line <- 1 }
  
  # Duplicates each line before the current header in the
  # output array we build up
  output <- c()
  if (newly_created) {
    output <- c(output, date_header)
  }
  else {
    if (current_date_line != 1) {
      output <- lines[1:current_date_line] 
    } else {
      
      # Checks for the case where the first line is the current date
      output <- c(output, date_header) 
    }
  }
  
  # Adds our new entry to the first open position under
  # the current date along with an empty line
  output <- c(output, entry)
  output <- c(output, "")
  
  # Joins the new entry and current date with the previous
  # entries in the log. Retains previous date header if
  # the previous date is on the first line
  if (!newly_created && (!is.na(match(date_header, lines)))) {
    current_date_line <- current_date_line + 1
  }
  output <- c(output, lines[current_date_line:length(lines)])
  
  # Records the update time
  output[length(output)] = update_header
  
  # Overwrites file with new output
  conn <- file(log_file)
  writeLines(output, conn)
  close(conn)
}

write_log("Documents/Histograms/activity_log_test.txt", "this is an update")


