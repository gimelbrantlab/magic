# Contains functions required by multiple scripts and library functions

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
# LIBRARIES AND SCRIPTS
######

# easy load data func
load_data <- function(training_path){
  if (grepl(".\\.csv", c(training_path))){
    training_set <- read.csv(training_path, header = TRUE)
  }else {training_set <- read.delim(training_path, header = TRUE)
  }
  return(training_set)
}

# load shiny libraries
load_shiny_libraries <- function(){
  get_package("shiny")
  get_package("markdown")
  get_package("shinythemes")
  get_package("GGally")
  get_package("PRROC")
  get_package("shinyFiles")
}

# Loads or installs all required packages for analysis
load_analyze_libraries <- function() {
  get_package("ggplot2")
  get_package("scales")
  get_package("randomForest")
  get_package("kernlab")
  get_package("caret")
  get_package("lattice")
  get_package("pROC")
  get_package("ada")
  get_package("fastAdaboost")
  get_package("mboost")
  get_package("randomForest")
  get_package("RSNNS")
  get_package("nnet")
  get_package("lattice")
  get_package("optparse")
  get_package("evtree")
  get_package("MASS")
  get_package("dplyr")
  get_package("parallel")
  get_package("e1071")
}

# Loads or installs all required packages for processing
load_process_libraries <- function() {
  get_package("plyr")
  get_package("dplyr")
  get_package("reshape2")
  get_package("kernlab")
  get_package("caret", dependencies = TRUE)
  get_package("lattice")
  get_package("parallel")
  get_package("diptest")
  get_package("doMC", repos = "http://R-Forge.R-project.org")
  get_package("e1071")
  get_package("gridExtra")
}

# Sources all scripts required for processing
load_process_scripts <- function(src_folder) {
  source(file.path(src_folder, "make_bed.R"))
  source(file.path(src_folder, "bigwig_to_scores.R"))
  source(file.path(src_folder, "normalize_scores.R"))
  source(file.path(src_folder, "join_input.R"))
}

# Loads or installs all required packages for machine learning
load_generate_libraries <- function() {
  get_package("scales", dependencies = TRUE)
  get_package("ggplot2", dependencies = TRUE)
  get_package("caret", dependencies = TRUE)
  get_package("doMC", repos = "http://R-Forge.R-project.org")
  get_package("pROC")
  get_package("ada")
  get_package("fastAdaboost")
  get_package("mboost")
  get_package("randomForest")
  get_package("RSNNS")
  get_package("nnet")
  get_package("kernlab")
  get_package("lattice")
  get_package("optparse")
  get_package("dplyr")
  get_package("evtree")
  get_package("MASS")
  get_package("e1071")
}

# Sources all scripts required for machine learning
load_generate_scripts <- function(src_folder) {
  source(file.path(src_folder, "scores_ml.R"))
  source(file.path(src_folder, "compare_ml.R"))
}

######
# UTILITY FUNCTIONS
######


# Checks if a package is installed, and installs it if specified.
# Also loads the package
get_package <- function(package_name, repos = "http://cran.us.r-project.org", 
                        dependencies = NA) {
  if(!is.element(package_name, installed.packages()[,1])) {
    print(package_name)
    if (repos == "") {
      install.packages(pkgs = package_name, dependencies = dependencies)
    } else {
      install.packages(pkgs = package_name, repos = repos)
    }
  }
  if(!suppressMessages(require(package_name, character.only = TRUE))) {
    stop(paste("Cannot load package", package_name))
  }
}

# Wrapper for cat to write to output file
cat_f <- function(s, file, append = TRUE) {
  cat(s, file = file, sep = "\n", append = append)
}

# Loads or installs all required packages for argument parsing
load_initial_libraries <- function() {
  get_package("optparse")
}

# Loads all file names matching a given pattern, with a name
# and a separator, into a list
get_names <- function(input_folder, pattern, sep = "_") {
  output <- list()
  files <- list.files(input_folder, pattern = pattern, recursive = FALSE)
  for (f in files) {
    output <- c(output, strsplit(f, sep)[[1]][1])
  }
  return(output)
}

# Adds id column to normalized scores and orders by id
attach_ids <- function(df) {
  df$id <- tolower(paste(df$name, df$chrom, sep = "_"))
  df <- df[order(df$id),]
  return(df)
}
