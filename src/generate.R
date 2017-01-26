#!/usr/bin/env Rscript

# Generates classifiers from data run through process.R

######
# MAIN FUNCTIONS
######


# Generates seven different classifiers via scores_ml.R on given scores file
generate_classifiers <- function(scores_file, output_folder, sampling_method,
                                 selection_rule, target_feature, p,
                                 metric, cv) {
  
  model_list <- c("glmStepAIC", "rf", "nnet", "rpart", "svmPoly",
                  "evtree", "knn", "ada", "mlpML")
  for (model_name in model_list) {
    scores_ml(scores_file, target_feature, model_name, 
              output_folder, selection_rule, sampling_method,
              p, metric, cv)
  }
}

# Generates classifiers from given training file
generate_main <- function(current_folder, input_file, output_folder,
                          sampling_method, selection_rule, target_feature,
                          p, metric, cv) {
  
  # Loads required scripts and libraries
  load_generate_libraries()
  load_generate_scripts(current_folder)
  
  # Converts percent to double between 0 and 1
  p <- p/100
  
  # Generates classifiers
  generate_classifiers(input_file, output_folder, sampling_method,
                       selection_rule, target_feature, p,
                       metric, cv)
  
  cat("Models generated. Generating model comparisons on resampled training data...\n")
  
  # Creates model comparison output folders and compares models
  comparison_folder <- file.path(output_folder, "comparisons")
  if (!dir.exists(comparison_folder)) { dir.create(comparison_folder) }
  compare_ml(output_folder, comparison_folder)
  
  cat("Model generation complete\n")
}

######
# COMMAND LINE INTERFACE
######


# Suppressingfire's SO solution to get executing script.
# Found at http://stackoverflow.com/questions/30468412/dplyr-join-warning-joining-factors-with-different-levels
args <- commandArgs()
current_folder <- dirname(sub("--file=", "", args[grep("--file=", args)]))

# Loads utility functions
models_folder <- file.path(current_folder, "..", "models")
source(file.path(current_folder, "utils.R"))

# Loads optparse
load_initial_libraries()

# Builds option list using optparse
options = list(
  make_option(c("-i", "--input_file"), type="character", default=NULL, 
              help="tsv file output by process.R, see readme for description"),
  make_option(c("-o", "--output_folder"), type="character", default="output", 
              help="output folder [default= %default]"),
  make_option(c("-t", "--target_feature"), type="character", default="status",
              help="name of column in dataset with feature to classify by [default= %default]"),
  make_option(c("-m", "--metric"), type="character", default="Kappa",
              help="metric to train on [default= %default]"),
  make_option(c("-s", "--sampling_method"), type="character", default="none",
              help="resampling method used to train classifiers [default= %default]"),
  make_option(c("-r", "--selection_rule"), type="character", default="best",
              help="caret rule used to select the best model [default= %default]"),
  make_option(c("-p", "--training_percent"), type="double", default=80,
              help="percent of data to use as training set [default= %default]"),
  make_option(c("-c", "--cross_validation"), type="integer", default=5,
              help="number of times to run cross-validation [default= %default]"),
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
p <- opt$training_percent
metric <- opt$metric
cv <- opt$cross_validation
quiet <- opt$quiet

# Calls main function, disabling output if running in quiet mode
if (!quiet) {
  invisible(generate_main(current_folder, input_file, output_folder, 
                          sampling_method, selection_rule, target_feature,
                          p, metric, cv))
} else {
  generate_main(current_folder, input_file, output_folder, 
                sampling_method, selection_rule, target_feature,
                p, metric, cv)
}