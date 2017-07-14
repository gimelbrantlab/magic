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
  
  # Opens connection to log and creates new log if necessary
  lines <- NA
  conn <- file(log_file)
  if (!file.exists(log_file)) {  
    writeLines(c("", paste("Activity log created on", current_date)), conn)
    lines <- readLines(conn)
  } else {
    lines <- readLines(conn)
  }
  close(conn)
  
  print(lines)
}

write_log("Documents/Histograms/activity_log_test.txt", "a")


