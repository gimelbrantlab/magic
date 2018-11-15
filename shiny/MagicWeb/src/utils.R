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
load_shiny_libraries <- function(lib.loc = NULL) {
  suppressMessages(library(shiny, lib.loc = lib.loc))
  suppressMessages(library(markdown, lib.loc = lib.loc))
  suppressMessages(library(shinythemes, lib.loc = lib.loc))
  suppressMessages(library(GGally, lib.loc = lib.loc))
  suppressMessages(library(PRROC, lib.loc = lib.loc))
  suppressMessages(library(shinyFiles, lib.loc = lib.loc))
  suppressMessages(library(shinyBS, lib.loc = lib.loc))
  suppressMessages(library(DT, lib.loc = lib.loc))
}

# Loads or installs all required packages for analysis
load_analyze_libraries <- function(lib.loc = NULL) {
  suppressMessages(library(methods))
  suppressMessages(library(ggplot2, lib.loc = lib.loc))
  suppressMessages(library(scales, lib.loc = lib.loc))
  suppressMessages(library(randomForest, lib.loc = lib.loc))
  suppressMessages(library(kernlab, lib.loc = lib.loc))
  suppressMessages(library(caret, lib.loc = lib.loc))
  suppressMessages(library(lattice, lib.loc = lib.loc))
  suppressMessages(library(pROC, lib.loc = lib.loc))
  suppressMessages(library(ada, lib.loc = lib.loc))
  suppressMessages(library(fastAdaboost, lib.loc = lib.loc))
  suppressMessages(library(mboost, lib.loc = lib.loc))
  suppressMessages(library(randomForest, lib.loc = lib.loc))
  suppressMessages(library(RSNNS, lib.loc = lib.loc))
  suppressMessages(library(nnet, lib.loc = lib.loc))
  suppressMessages(library(optparse, lib.loc = lib.loc))
  suppressMessages(library(evtree, lib.loc = lib.loc))
  suppressMessages(library(MASS, lib.loc = lib.loc))
  suppressMessages(library(dplyr, lib.loc = lib.loc))
  suppressMessages(library(foreach, lib.loc = lib.loc))
  suppressMessages(library(iterators, lib.loc = lib.loc))
  suppressMessages(library(parallel, lib.loc = lib.loc))
  suppressMessages(library(e1071, lib.loc = lib.loc))
}

# Loads or installs all required packages for processing
load_process_libraries <- function(lib.loc = NULL) {
  suppressMessages(library(methods))
  suppressMessages(library(plyr, lib.loc = lib.loc))
  suppressMessages(library(dplyr, lib.loc = lib.loc))
  suppressMessages(library(reshape2, lib.loc = lib.loc))
  suppressMessages(library(kernlab, lib.loc = lib.loc))
  suppressMessages(library(ggplot2, lib.loc = lib.loc))
  suppressMessages(library(caret, lib.loc = lib.loc))
  suppressMessages(library(lattice, lib.loc = lib.loc))
  suppressMessages(library(foreach, lib.loc = lib.loc))
  suppressMessages(library(iterators, lib.loc = lib.loc))
  suppressMessages(library(parallel, lib.loc = lib.loc))
  suppressMessages(library(diptest, lib.loc = lib.loc))
  suppressMessages(library(doMC, lib.loc = lib.loc))
  suppressMessages(library(e1071, lib.loc = lib.loc))
  suppressMessages(library(gridExtra, lib.loc = lib.loc))
}

# Loads or installs all required packages for machine learning
load_generate_libraries <- function(lib.loc = NULL) {
  suppressMessages(library(methods))
  suppressMessages(library(scales, lib.loc = lib.loc))
  suppressMessages(library(ggplot2, lib.loc = lib.loc))
  suppressMessages(library(caret, lib.loc = lib.loc))
  suppressMessages(library(foreach, lib.loc = lib.loc))
  suppressMessages(library(iterators, lib.loc = lib.loc))
  suppressMessages(library(doMC, lib.loc = lib.loc))
  suppressMessages(library(pROC, lib.loc = lib.loc))
  suppressMessages(library(ada, lib.loc = lib.loc))
  suppressMessages(library(fastAdaboost, lib.loc = lib.loc))
  suppressMessages(library(mboost, lib.loc = lib.loc))
  suppressMessages(library(randomForest, lib.loc = lib.loc))
  suppressMessages(library(RSNNS, lib.loc = lib.loc))
  suppressMessages(library(nnet, lib.loc = lib.loc))
  suppressMessages(library(kernlab, lib.loc = lib.loc))
  suppressMessages(library(lattice, lib.loc = lib.loc))
  suppressMessages(library(optparse, lib.loc = lib.loc))
  suppressMessages(library(dplyr, lib.loc = lib.loc))
  suppressMessages(library(evtree, lib.loc = lib.loc))
  suppressMessages(library(MASS, lib.loc = lib.loc))
  suppressMessages(library(e1071, lib.loc = lib.loc))
}

# Sources all scripts required for processing
load_process_scripts <- function(src_folder) {
  source(file.path(src_folder, "make_bed.R"))
  source(file.path(src_folder, "bigwig_to_scores.R"))
  source(file.path(src_folder, "normalize_scores.R"))
  source(file.path(src_folder, "join_input.R"))
}

# Sources all scripts required for machine learning
load_generate_scripts <- function(src_folder) {
  source(file.path(src_folder, "scores_ml.R"))
  source(file.path(src_folder, "compare_ml.R"))
}

# Loads in packages from custom install directory if saved via install.R
get_install_dir <- function(main_folder) {
  install_data <- file.path(main_folder, "install_data.txt")
  if (file.exists(install_data)) {
    lib <- readLines(install_data)[1]
    return(lib)
  } else {
    return(NULL)
  }
}


######
# UTILITY FUNCTIONS
######

# Wrapper for cat to write to output file
cat_f <- function(s, file, append = TRUE) {
  cat(s, file = file, sep = "\n", append = append)
}

# Loads all required packages for argument parsing
load_initial_libraries <- function(lib.loc = NULL) {
  suppressMessages(library(optparse, lib.loc = lib.loc))
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

