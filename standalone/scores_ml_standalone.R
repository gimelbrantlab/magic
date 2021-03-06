#!/usr/bin/env Rscript

# Generates classifiers for scores data output from predict_mae.R

######
### LIBRARIES
######


# Loads or installs all required packages
load_libraries <- function() {
  get_package("caret", dependencies = TRUE)
  get_package("doMC", repos = "http://R-Forge.R-project.org")
  get_package("pROC")
  get_package("ada")
  get_package("mboost")
  get_package("randomForest")
  get_package("neuralnet")
  get_package("kernlab")
  get_package("lattice")
  get_package("optparse")
  get_package("dplyr")
  get_package("evtree")
}


######
### UTILITY FUNCTIONS
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

# Wrapper for sink to write an object to a file
sink_f <- function(s, file, append = TRUE) {
  sink(file, append)
  print(s)
  sink(NULL)
}

######
### MAIN FUNCTIONS
######


# Generates the training and testing sets for the given data frame.
# Returns list of [training, testing]
generate_sets <- function(df, target_feature, seed = 50, split = 0.8) {
  
  # Sets seed to make this reproducible
  set.seed(seed)
  
  # Splits data using cutoff indice for given percent of sample size
  cutoff <- createDataPartition(df[target_feature], p = split,
                                list = FALSE, times = 1)
  training <- df[cutoff,]
  test <- df[-cutoff,]
  
  # Returns list with given dataframes
  return(list(training, test))
}

# Runs machine learning analysis on training data using the specified method
generate_model <- function(training, method, target_feature, log_file,
                           selection_rule, seed = 50) {
  
  # Sets seed to make this reproducible and gets begin time
  set.seed(seed)
  start_time <- Sys.time()
  
  # Only keeps certain features
  h3k27me3_cols <- c("h3k27me3_body_percentile",
                     "h3k27me3_promoter_percentile",
                     "h3k27me3_body_norm_sum",
                     "h3k27me3_promoter_norm_sum")
  h3k36me3_cols <- c("h3k36me3_body_percentile",
                     "h3k36me3_promoter_percentile",
                     "h3k36me3_body_norm_sum",
                     "h3k36me3_promoter_norm_sum")
  h3k9me2_cols <- c("h3k9me2_body_percentile",
                    "h3k9me2_promoter_percentile",
                    "h3k9me2_body_norm_sum",
                    "h3k9me2_promoter_norm_sum")
  cpg_cols <- c("cpg_body_percentile",
                "cpg_promoter_percentile",
                "cpg_body_norm_sum",
                "cpg_promoter_norm_sum")
  cols_to_keep <- c(target_feature,
                    h3k27me3_cols,
                    h3k36me3_cols,
                    h3k9me2_cols,
                    cpg_cols
                    )
  #training <- training[, colnames(training) %in% cols_to_keep]
  
  # Gets training formula
  train_formula <- as.formula(paste(target_feature, " ~ .", sep = ""))
  
  # Sets up 5-fold cross-validation training control method and trains model.
  # If only two classes, uses ROC instead of accuracy and kappa
  train_control <- NULL
  model <- NULL
  if (nlevels(training[[target_feature]]) == 2) {
    train_control <- trainControl(method = "cv",
                                  number = 5,
                                  summaryFunction = twoClassSummary,
                                  selectionFunction = selection_rule,
                                  classProbs = TRUE,
                                  savePredictions = TRUE)
    model <- train(train_formula,
                   data = training,
                   method = method,
                   metric = "ROC",
                   na.action = na.omit,
                   trControl = train_control)
  } else {
    train_control <- trainControl(method = "cv",
                                  number = 5,
                                  selectionFunction = selection_rule,
                                  savePredictions = TRUE)
    model <- train(train_formula,
                   data = training,
                   method = method,
                   na.action = na.omit,
                   trControl = train_control)
  }
  
  # Prints total time required
  sink_f(paste(method, "time taken:", Sys.time() - start_time), log_file)
  
  # Explicitly returns model
  return(model)
}

# Wrapper for generate_model to generate a single classifer, save it as an Rdata
# file and print a summary of it to a text file
generate_classifier <- function(training, testing, classifier, target_feature,
                                output_folder, log_file, selection_rule) {
  
  # Generates model and logs stats to file
  model <- generate_model(training, classifier, target_feature, log_file,
                          selection_rule)
  sink_f(model, log_file)
  
  # Saves model to file
  model_file <- file.path(output_folder, 
                          paste(classifier, "_model.rds", sep = ""))
  saveRDS(model, file = model_file)
  
  # Generates predictions on testing data and writes confusion matrix to file
  predictions <- predict(model, testing)
  sink_f("", log_file)
  sink_f(postResample(predictions, testing[[target_feature]]), log_file)
  if (nlevels(training[[target_feature]]) == 2) {
    sink_f(paste("sensitivity:", sensitivity(predictions, testing[[target_feature]])), 
           log_file)
    sink_f(paste("specificity:", specificity(predictions, testing[[target_feature]])), 
           log_file)
    cMat <- as.table(confusionMatrix(predictions, testing[[target_feature]]))
    matrix_file <- file.path(output_folder, paste(classifier, "matrix", sep = "_"))
    write.table(cMat, file = matrix_file, quote = FALSE)
  }
  
  # Writes predictions to file
  testing$predictions <- predictions
  testing_file <- file.path(output_folder, paste(classifier, "_testing.tsv", sep = ""))
  write.table(testing, file = testing_file, sep = "\t", 
              quote = FALSE, row.names = FALSE, col.names = TRUE)
}

# Runs machine learning analyses on data
scores_ml <- function(scores_file, target_feature, method, output_folder,
                      selection_rule, clean = FALSE, cores = 4) {
  
  # Creates output folder if it doesn't exist
  if (!dir.exists(output_folder)) { dir.create(output_folder) }
  
  # Resets summary file
  summary_file <- file.path(output_folder, 
                            paste(method, "summary.txt", sep = "_"))
  sink_f("Summary file", summary_file, FALSE)
  
  # Enables multicore processing using doMC package
  registerDoMC(cores = cores)
  
  # Loads scores file into a data frame and removes unnecesary cols
  df <- read.csv(scores_file, sep = "\t")
  df <- df[, !(names(df) %in% c("start", "end", "chrom", "name"))]
  
  # Splits data into training and testing sets
  partition <- createDataPartition(df$status, times = 1, p = 0.8, list = FALSE)
  training <- df[partition,]
  testing <- df[-partition,]
  
  # Writes training and testing dataframes to output folder
  write.table(testing, file = file.path(output_folder, "testing_set.csv"),
              sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
  
  # Generates model and saves it to output folder
  generate_classifier(training, testing, method, target_feature, 
                      output_folder, summary_file, selection_rule)
}

##################
# COMMAND LINE INTERFACE
##################


# Loads required libraries
load_libraries()

# Builds option list using optparse
options = list(
  make_option(c("-i", "--input_file"), type="character", default=NULL, 
              help="percentiles generated from predict_mae.R"),
  make_option(c("-o", "--output_folder"), type="character", default="output", 
              help="path to output folder"),
  make_option(c("-f", "--target_feature"), type="character", default="status", 
              help="target feature for classification [default= %default]"),
  make_option(c("-m", "--method"), type="character", default="rf", 
              help="classification method to use [default= %default]"),
  make_option(c("-s", "--selection_rule"), type="character", default="best", 
              help="model selection method to use [default= %default]"),
  make_option(c("-k", "--cores"), type="integer", default=4, 
              help="number of cores to run on [default= %default]")
)

# Gets options and checks arguments
opt <- parse_args(OptionParser(option_list = options))
if (!file.exists(opt$input_file)) { stop("input file does not exist") }
if (!dir.exists(opt$output_folder)) { dir.create(opt$output_folder) }

# Extracts variables from args
input_file <- opt$input_file
output_folder <- opt$output_folder
target_feature <- opt$target_feature
method <- opt$method
selection_rule <- opt$selection_rule
cores <- opt$cores

# Calls main function
scores_ml(input_file, target_feature, method, output_folder,
          selection_rule, cores)