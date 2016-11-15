#!/usr/bin/env Rscript

# Generates classifiers from data run through process.R

######
# MAIN FUNCTIONS
######


# Generates seven different classifiers via scores_ml.R on given scores file
generate_classifiers <- function(scores_file, output_folder, sampling_method,
                                 selection_rule, target_feature) {
  model_list <- c("glmStepAIC", "rf", "nnet", "rpart", "svmPoly",
                  "evtree", "knn", "ada", "mlpML")
  for (model_name in model_list) {
    scores_ml(scores_file, target_feature, model_name, output_folder,
              selection_rule, sampling_method)
  }
}

# Generates classifiers from given training file
generate_main <- function(current_folder, input_file, output_folder,
                          sampling_method, selection_rule, target_feature) {
  
  # Loads required scripts and libraries
  load_generate_libraries()
  load_generate_scripts(current_folder)
  
  # Generates classifierss
  generate_classifiers(input_file, output_folder, sampling_method,
                       selection_rule, target_feature)
  
  cat("Models generated. Generating model comparisons on resampled training data...\n")
  
  # Creates model comparison output folders and compares models
  comparison_folder <- file.path(output_folder, "comparisons")
  if (!dir.exists(comparison_folder)) { dir.create(comparison_folder) }
  compare_ml(output_folder, comparison_folder)
  
  cat("Finished\n")
}

######
# COMMAND LINE INTERFACE
######


# Suppressingfire's SO solution to get executing script.
# Found at http://stackoverflow.com/questions/30468412/dplyr-join-warning-joining-factors-with-different-levels
args <- commandArgs()
current_folder <- dirname(sub("--file=", "", args[grep("--file=", args)]))

# Sets working directory to src folder, gets path to models folder
# and loads utility functions
setwd(file.path("..", current_folder))
source("utils.R")

# Loads optparse
load_initial_libraries()

# Builds option list using optparse
options = list(
  make_option(c("-i", "--input_file"), type="character", default=NULL, 
              help="tsv file output by process.R, see readme for description"),
  make_option(c("-o", "--output_folder"), type="character", default="output", 
              help="output folder [default= %default]"),
  make_option(c("-sa", "--sampling_method"), type="character", default="none",
              help="resampling method used to train classifiers [default= %default]"),
  make_option(c("-se", "--selection_rule"), type="character", default="best",
              help="caret rule used to select the best model [default= %default]"),
  make_option(c("-ta", "--target_feature"), type="character", default="status",
              help="name of column in dataset with feature to classify by [default= %default]"),
  make_option(c("-q", "--quiet"), action="store_true", default=FALSE, 
              help="disables console output [default= %default]")
)

# Gets options and checks arguments
opt <- parse_args(OptionParser(option_list = options))
if (!file.exists(opt$input_file)) { stop("input file does not exist") }
if (!dir.exists(opt$output_folder)) { dir.create(opt$output_folder) }

# Extracts variables from args
input_file <- opt$input_file
output_folder <- opt$output_folder
sampling_method <- opt$sampling_method
selection_rule <- opt$selection_rule
target_feature <- opt$target_feature
quiet <- opt$quiet

# Calls main function, disabling output if running in quiet mode
if (!quiet) {
  invisible(generate_main(current_folder, input_file, output_folder, 
                          sampling_method, selection_rule, target_feature))
} else {
  generate_main(current_folder, input_file, output_folder, 
                sampling_method, selection_rule, target_feature)
}