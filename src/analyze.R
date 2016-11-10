#!/usr/bin/env Rscript

# Analyzes data processed by process.R

######
# LIBRARIES AND SCRIPTS
######

# Loads or installs all required packages
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

######
### MAIN FUNCTIONS
######


# Loads all prediction sets in a given folder into a single dataframe
join_prediction_sets <- function(df, predictions_folder) {
  
  # List of prediction sets to build up
  df$id <- 1:nrow(df)
  sets <- df
  
  # Loops through all files and reads in all ending with "_predictions.tsv"
  sets_files <- list.files(predictions_folder, pattern = "*_predictions.tsv", recursive = FALSE)
  for (file in sets_files) {
    p_set <- read.csv(file.path(predictions_folder, file), sep = "\t")
    
    # Adds id col to set
    p_set$id <- 1:nrow(p_set)
    
    # Renames cols with model name prepended
    model_name <- strsplit(file, "_")[[1]][1]
    names_to_change <- names(p_set)[names(p_set) != "id"]
    names(p_set)[names(p_set) != "id"] <- 
      paste(model_name, names_to_change, sep = "_")
    
    # Joins sets
    sets <- suppressWarnings(full_join(sets, p_set, by = "id"))
  }
  
  # Removes id column from output
  sets <- sets[,names(p_set) != "id"]
  
  # Explicitly returns list of testing sets
  return(sets)
}

# Loads all models in a given folder into a dict
get_models <- function(input_folder) {
  
  # List of models to build up
  models <- list()
  
  # Loops through all files and reads in all ending with "_model.rds"
  model_files <- list.files(input_folder, pattern = "*_model.rds", recursive = FALSE)
  for (file in model_files) {
    model_name <- strsplit(file, "_")[[1]][1]
    model <- readRDS(file.path(input_folder, file))
    models[[model_name]] = model
  }
  
  # Explicitly returns list of models
  return(models)
}

# Generates predictions for a single model and logs those to file
ml_predict_inner <- function(model_name, model, output_folder, 
                             run_name, positive_class) {
  
  # Generates predictions and writes to file
  predictions <- predict(model, df, positive = positive_class)
  predictions_file <- file.path(output_folder, 
                                paste(model_name, "predictions.tsv", sep = "_"))
  write.table(df, predictions_file, sep = "\t", row.names = FALSE,
              quote = FALSE)
}

# Generates predictions on new data
ml_predict <- function(df, models_folder, output_folder, 
                       positive_class) {
  
  # Loads all models into a dict with model names as keys
  models <- get_models(models_folder)
  
  # Generates predictions on df for all models
  for (model_name in names(models)) {
    ml_predict_inner(model_name, models[[model_name]], output_folder, 
                     positive_class)
  }
  
  # Appends predictions to dataframe and returns it
  df <- join_prediction_sets(df, output_folder)
  return(df)
}

# Analyzes given dataset
analyze_main <- function(current_folder, input_file, models_folder, 
                         output_folder, positive_class) {
  
  # Loads required libraries
  load_analyze_libraries()
  
  # Reads input file into df and removes NA values
  df <- read.csv(input_file, sep = "\t")
  df <- df[complete.cases(df),]
  
  # Generates predictions and appends them to df
  df <- ml_predict(df, models_folder, output_folder, positive_class)
}

##################
# COMMAND LINE INTERFACE
##################


# Loads optparse
load_initial_libraries()

# Suppressingfire's SO solution to get executing script.
# Found at http://stackoverflow.com/questions/30468412/dplyr-join-warning-joining-factors-with-different-levels
args <- commandArgs()
current_folder <- dirname(sub("--file=", "", args[grep("--file=", args)]))

# Sets working directory to src folder and gets path to models folder
setwd(file.path("..", current_folder))
models_folder <- file.path("models")

# Builds option list using optparse
options = list(
  make_option(c("-i", "--input_file"), type="character", default=NULL, 
              help="file output from process.R, see readme for description"),
  make_option(c("-m", "--models_folder"), type="character", default=models_folder, 
              help="output folder from generate.R"),
  make_option(c("-o", "--output_folder"), type="character", default="analyze_output", 
              help="output folder [default= %default]"),
  make_option(c("-po", "--positive_class"), type="character", default="MAE",
              help="name of target feature's positive class [default=%default]"),
  make_option(c("-q", "--quiet"), action="store_true", default=FALSE, 
              help="disables console output [default= %default]")
)

# Gets options and checks arguments
opt <- parse_args(OptionParser(option_list = options))
if (!dir.exists(opt$input_file)) { stop("input file does not exist") }
if (!dir.exists(opt$models_folder)) { stop("models folder does not exist") }
if (!dir.exists(opt$output_folder)) { dir.create(opt$output_folder) }

# Extracts variables from args
input_file <- opt$input_file
models_folder <- opt$models_folder
output_folder <- opt$output_folder
quiet <- opt$quiet
positive_class <- opt$positive_class

# Calls main function, disabling output if running in quiet mode
if (!quiet) {
  invisible(analyze_main(current_folder, input_file, models_folder, 
                         output_folder, positive_class))
} else {
  analyze_main(current_folder, input_file, models_folder, 
               output_folder, positive_class)
}
