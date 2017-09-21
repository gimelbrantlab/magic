# Generates classifiers for scores data output from predict_mae.R

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
### UTILITY FUNCTIONS
######


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
generate_model <- function(training, classifier, target_feature, log_file,
                           selection_rule, sampling_method, metric, 
                           cv, seed = 50) {
  
  # Sets seed to make this reproducible and gets begin time
  set.seed(seed)
  start_time <- Sys.time()
  
  # Gets training formula
  train_formula <- as.formula(paste(target_feature, " ~ .", sep = ""))
  
  # Checks if metric in list of twoClassSummary function's metrics
  two_class_metrics <- c("ROC", "Sens", "Spec")
  
  # Sets up cross-validation training control method and trains model.
  # If only two classes, uses ROC instead of accuracy and kappa
  train_control <- NULL
  model <- NULL
  if (nlevels(training[[target_feature]]) == 2) {
    
    # Caret doesn't like passing in "none" as the sampling parameter
    if (sampling_method != "none") {
      
      # Detects whether or not a twoClassSummary function is appropriate for
      # the metric to train on
      if (metric %in% two_class_metrics) {
        train_control <- trainControl(method = "cv",
                                      number = cv,
                                      summaryFunction = twoClassSummary,
                                      selectionFunction = selection_rule,
                                      sampling = sampling_method,
                                      classProbs = TRUE,
                                      savePredictions = TRUE)
      } else {
        train_control <- trainControl(method = "cv",
                                      number = cv,
                                      selectionFunction = selection_rule,
                                      sampling = sampling_method,
                                      classProbs = TRUE,
                                      savePredictions = TRUE)
      }
    } else {
      if (metric %in% two_class_metrics) {
        train_control <- trainControl(method = "cv",
                                      number = cv,
                                      summaryFunction = twoClassSummary,
                                      selectionFunction = selection_rule,
                                      classProbs = TRUE,
                                      savePredictions = TRUE)
      } else {
        train_control <- trainControl(method = "cv",
                                      number = cv,
                                      selectionFunction = selection_rule,
                                      classProbs = TRUE,
                                      savePredictions = TRUE)
      }
    }
    capture.output(model <- caret::train(train_formula,
                                         data = training,
                                         method = classifier,
                                         metric = metric,
                                         na.action = na.omit,
                                         trControl = train_control))
  } else {
    train_control <- trainControl(method = "cv",
                                  number = cv,
                                  selectionFunction = selection_rule,
                                  savePredictions = TRUE)
    capture.output(model <- caret::train(train_formula,
                                         data = training,
                                         method = classifier,
                                         metric = metric,
                                         na.action = na.omit,
                                         trControl = train_control))
  }
  
  # Prints total time required
  sink_f(paste(classifier, "time taken:", Sys.time() - start_time), log_file)
  
  # Explicitly returns model
  return(model)
}

# Wrapper for generate_model to generate a single classifer, save it as an Rdata
# file and print a summary of it to a text file
generate_classifier <- function(training, testing, testing_gene_names, classifier, 
                                target_feature, output_folder, log_file, 
                                selection_rule, sampling_method, metric,
                                cv) {
  
  # Generates model and logs stats to file
  model <- generate_model(training, classifier, target_feature, log_file,
                          selection_rule, sampling_method, metric,
                          cv)
  sink_f(model, log_file)
  
  # Saves model to file
  model_file <- file.path(output_folder, 
                          paste(classifier, "_model.rds", sep = ""))
  saveRDS(model, file = model_file)
  
  # Generates predictions on testing data and writes confusion matrix to file.
  # Only run if testing set exists
  if (!is.null(testing)) {
    predictions <- predict(model, testing)
    cat_f("\n", log_file)
    sink_f(postResample(predictions, testing[[target_feature]]), log_file)
    if (nlevels(training[[target_feature]]) == 2) {
      sens <- sensitivity(predictions, testing[[target_feature]], positive = "MAE")
      spec <- specificity(predictions, testing[[target_feature]], negative = "BAE")
      cat_f(paste("sensitivity:", sens, "\n"), log_file)
      cat_f(paste("specificity:", spec, "\n"), log_file)
      cMat <- as.table(caret::confusionMatrix(predictions, testing[[target_feature]]),
                       positive = "MAE")
      matrix_file <- file.path(output_folder, paste(classifier, "matrix.txt", sep = "_"))
      write.table(cMat, file = matrix_file, quote = FALSE)
    }
    
    # Writes predictions appended to testing set to file
    testing$prediction <- predictions
    testing$name <- testing_gene_names
    testing_file <- file.path(output_folder, paste(classifier, "_testing.tsv", sep = ""))
    write.table(testing, file = testing_file, sep = "\t", 
                quote = FALSE, row.names = FALSE, col.names = TRUE)
  }
}

# Runs machine learning analyses on data
scores_ml <- function(training, testing, testing_gene_names, target_feature, classifier, output_folder,
                      selection_rule, sampling_method = "none", p = 0.8, 
                      metric = "Kappa", cv = 5, cores = 4) {
  
  # Creates summary file with header
  summary_file <- file.path(output_folder, 
                            paste(classifier, "summary.txt", sep = "_"))
  cat_f(paste(classifier, "summary\n\n"), summary_file, FALSE)
  
  # Enables multicore processing using doMC package
  registerDoMC(cores = cores)
  
  # Generates model and saves it to output folder
  generate_classifier(training, testing, testing_gene_names, classifier, 
                      target_feature, output_folder, summary_file, 
                      selection_rule, sampling_method, metric,
                      cv)
}
