#!/usr/bin/env Rscript

# Generates classifiers from data run through process.R

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
                                 metric, cv, model_string) {
  
  model_list <- strsplit(model_string, ",")    
  for (model_name in model_list) {
    scores_ml(scores, target_feature, model_name, 
              output_folder, selection_rule, sampling_method,
              p, metric, cv)
  }
}

# Generates classifiers from given training file
generate_main <- function(current_folder, input_file, output_folder,
                          sampling_method, selection_rule, target_feature,
                          training_genes_file, p, metric, 
                          cv,model_string) {
  
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
    training_genes_file <- file.path(reference_folder, training_genes_file)
    if (!file.exists(training_genes_file)) {
      stop("training genes file does not exist (use 'mouse', 'human' or 'none')")
    }
  }
  
  # Loads input file into a dataframe and attaches ids to it
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
                       metric, cv, model_string)
  
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
quiet <- opt$quiet

# Calls main function, disabling output if running in quiet mode
if (!quiet) {
  invisible(generate_main(current_folder, input_file, output_folder, 
                          sampling_method, selection_rule, target_feature,
                          training_genes_file, p, metric, 
                          cv, model_string))
} else {
  generate_main(current_folder, input_file, output_folder, 
                sampling_method, selection_rule, target_feature,
                training_genes_file, p, metric, 
                cv, model_string)
}