
# Contains functions required by multiple scripts and loading functions

######
# LIBRARIES AND SCRIPTS
######


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
  get_package("kernlab")
  get_package("lattice")
  get_package("optparse")
  get_package("evtree")
  get_package("MASS")
  get_package("dplyr")
}

# Loads or installs all required packages for processing
load_process_libraries <- function() {
  get_package("plyr")
  get_package("dplyr")
  get_package("kernlab")
  get_package("caret", dependencies = TRUE)
  get_package("lattice")
  get_package("doMC", repos = "http://R-Forge.R-project.org")
}

# Sources all scripts required for processing
load_process_scripts <- function(src_folder) {
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
get_package <- function(package_name, repos = "", dependencies = FALSE) {
  if(!is.element(package_name, installed.packages()[,1])) {
    if (repos == "") {
      install.packages(package_name, dependencies)
    } else {
      install.packages(package_name, repos)
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