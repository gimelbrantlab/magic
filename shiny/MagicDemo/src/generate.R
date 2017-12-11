#!/usr/bin/env Rscript

# Generates classifiers from data run through process.R

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


# Loads training genes and attaches them to scores
attach_training_genes <- function(df, training_genes_file) {
  
  # Opens training genes
  training_genes <- read.csv(training_genes_file, sep = "\t")
  
  # Keeps only genes in both datasets
  training_genes$id <- 
    tolower(paste(training_genes$gene, training_genes$chrom, sep = "_"))
  ids_to_keep <- intersect(training_genes$id, df$id)
  df <- df[df$id %in% ids_to_keep, ]
  training_genes <- training_genes[training_genes$id %in% ids_to_keep, ]
  
  # Appends training genes to modified df
  df <- df[order(df$id),]
  training_genes <- training_genes[order(training_genes$id),]
  df$status <- training_genes$status
  
  # Explicitly returns modified scores df
  return(df)
}


# Generates seven different classifiers via scores_ml.R on given scores file
generate_classifiers <- function(scores, output_folder, sampling_method,
                                 selection_rule, target_feature, p,
                                 metric, cv, model_string, validation_file) {
  
  model_string <- gsub(" ", "", model_string)
  model_string <- gsub("\n", "", model_string)
  model_list <- strsplit(model_string, ",") 
  
  # Creates output folder if it doesn't exist
  if (!dir.exists(output_folder)) { dir.create(output_folder) }
  
  # Subset the data into training and testing
  
  ## Splits data into training and testing sets if 0 < p < 1
  df <- scores
  testing_gene_names <- NULL
  training <- NULL
  testing <- NULL
  if ((p > 0) && (p < 1)) {
    
    # Sets p% of data as training set
    partition <- createDataPartition(df$status, times = 1, p = p, list = FALSE)
    training <- df[partition,]
    testing <- df[-partition,]
    
    # Removes unnecessary cols from both sets, saving testing gene names
    testing_gene_names <- testing$name
    training <- training[, !(names(training) %in% c("start", "end", "chrom", "name"))]
    testing <- testing[, !(names(testing) %in% c("start", "end", "chrom", "name"))]
    
    # Uses all of the data as the training set if specified, otherwise throws error
  } else if (p == 0) {
    stop("must input p in range 0 < p <= 1")
  } else {
    training <- df
    training <- training[, !(names(training) %in% c("start", "end", "chrom", "name"))]
  }
  
  # Make sure we have file for validation
  if (!is.null(validation_file)){
    validation_df <- read.table(validation_file, header = T)
  }
  else if (((p > 0) && (p < 1))) {
    validation_df <- testing
  }
  else {
    warning("using 100% of training data for training and no validation file specified, no summary file will be created")
    validation_df <- NULL
  }
  
  # Create vector with stats for validaton
  if (!is.null(validation_df)){
    stats <- data.frame(Sensitivity = numeric(0), Specificity = numeric(0), PPV = numeric(0), NPV = numeric(0), F1 = numeric(0), Kappa = numeric(0), Accuracy = numeric(0), Balanced_Accuracy = numeric(0), stringsAsFactors = FALSE)
  }
  
  for (model_name in model_list[[1]]) {
    scores_ml(training, testing, testing_gene_names, target_feature, model_name, 
              output_folder, selection_rule, sampling_method,
              p, metric, cv, testing_file)
    if (!is.null(validation_df)) {
      metrics_out <- validation(model_name, output_folder, validation_df)
      stats <- rbind(stats, setNames(as.list(metrics_out), names(stats)))
    }
  }
  if (!is.null(validation_df)) {
    stats <- round(stats,3)
    stats <- cbind(model_list[[1]], stats)
    colnames(stats)[1] <- "Model"
    models_stats <- file.path(output_folder, "summary_models.tsv")
    write.table(stats, file=models_stats, quote = F, row.names = F, sep="\t")
  }
}

validation <- function(model_name, output_folder, validation_set) {
  # read the model generated just before
  model_file <- file.path(output_folder, 
                          paste(model_name, "_model.rds", sep = ""))
  model <- readRDS(model_file)
  # run comparison
  predictions <- predict(model, validation_set)
  validation_set[["status"]] <- sub("BAE", 0, validation_set[["status"]])
  validation_set[["status"]] <- sub("MAE", 1, validation_set[["status"]])
  predictions <- sub("BAE", 0, predictions)
  predictions <- sub("MAE", 1, predictions)
  # print output
  model_to_valid <- file.path(output_folder, 
                          paste(model_name, "_to_validation.txt", sep = ""))
  cm <- caret::confusionMatrix(predictions, validation_set[["status"]], positive = "1")
  capture.output(cm, file = model_to_valid, append = F)
  out <- c(cm$byClass[1:4], cm$byClass[7], cm$overall[2], cm$overall[1], cm$byClass[11])
  return(out)
}

# Generates classifiers from given training file
generate_main <- function(current_folder, input_file, output_folder,
                          sampling_method, selection_rule, target_feature,
                          training_genes_file, p, metric, 
                          cv, model_string, validation_file) {
  
  # Loads required scripts and libraries
  load_generate_libraries()
  load_generate_scripts(current_folder)
  
  # Gets reference folder based on location of executing script
  reference_folder <- file.path(dirname(current_folder), "reference")
  
  # Gets correct training genes file depending on species
  if (tolower(training_genes_file) == "mouse") {
    training_genes_file <- file.path(reference_folder, "mouse_tg.tsv")
  } else if (tolower(training_genes_file) == "human") {
    training_genes_file <- file.path(reference_folder, "human_tg.tsv")
  } else if (tolower(training_genes_file) == "none") {
    training_genes_file <- "none"
  } else {
    training_genes_file <- file.path(training_genes_file)
    if (!file.exists(training_genes_file)) {
      stop("training genes file does not exist (use 'mouse', 'human' or 'none')")
    }
  }
  
  # Loads input file into a dataframe
  scores <- read.csv(input_file, sep = "\t")
  
  # Converts percent to double between 0 and 1
  p <- p/100
  
  cat("Subsetting data to match the given training genes...\n")
  
  if (training_genes_file != "none") {
    scores <- attach_ids(scores)
    scores <- attach_training_genes(scores, training_genes_file)
    scores <- scores[, !(names(scores) %in% c("id"))]
  }
  
  cat("Data subset. Generating models...\n")
  
  # Generates classifiers
  generate_classifiers(scores, output_folder, sampling_method,
                       selection_rule, target_feature, p,
                       metric, cv, model_string, validation_file)
  
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
  make_option(c("-a", "--training_genes_file"), type="character", default="none", 
              help="'mouse', 'human' or 'none' for training genes set to use [default= %default]"),
  make_option(c("-s", "--sampling_method"), type="character", default="none",
              help="resampling method used to train classifiers [default= %default]"),
  make_option(c("-r", "--selection_rule"), type="character", default="best",
              help="caret rule used to select the best model [default= %default]"),
  make_option(c("-p", "--training_percent"), type="double", default=80,
              help="percent of data to use as training set [default= %default]"),
  make_option(c("-c", "--cross_validation"), type="integer", default=5,
              help="number of times to run cross-validation [default= %default]"),
  make_option(c("-v", "--validation_file"), type="character", default=NULL, 
              help="tsv file with genes and matching BAE/MAE status and chromatin data, see readme for description"),
  make_option(c("-q", "--quiet"), action="store_true", default=FALSE, 
              help="disables console output [default= %default]"),
  make_option(c("-l", "--model_list"), type="character", default = "glmStepAIC, rf, nnet, rpart, svmPoly,
                  evtree, knn, ada, mlpML",
              help="list of model algorithms to test, see readme for naming conventions [default= %default]")
)

# Gets options and checks arguments
opt <- parse_args(OptionParser(option_list = options))
if (!file.exists(opt$input_file)) { stop("input file does not exist") }
if (!dir.exists(opt$output_folder)) { dir.create(opt$output_folder, recursive = TRUE) }

# Extracts variables from args
input_file <- opt$input_file
output_folder <- opt$output_folder
sampling_method <- opt$sampling_method
selection_rule <- opt$selection_rule
target_feature <- opt$target_feature
p <- opt$training_percent
metric <- opt$metric
training_genes_file <- tolower(opt$training_genes_file)
cv <- opt$cross_validation
validation_file <- opt$validation_file
quiet <- opt$quiet
model_string <- opt$model_list

# Calls main function, disabling output if running in quiet mode
if (!quiet) {
  invisible(generate_main(current_folder, input_file, output_folder, 
                          sampling_method, selection_rule, target_feature,
                          training_genes_file, p, metric, 
                          cv, model_string, validation_file))
} else {
  generate_main(current_folder, input_file, output_folder, 
                sampling_method, selection_rule, target_feature,
                training_genes_file, p, metric, 
                cv, model_string, validation_file)
}
