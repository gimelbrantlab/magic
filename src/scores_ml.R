
# Generates classifiers for scores data output from predict_mae.R

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
                           selection_rule, sampling_method, seed = 50) {
  
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
                                  sampling = sampling_method,
                                  classProbs = TRUE,
                                  savePredictions = TRUE)
    capture.output(model <- train(train_formula,
                                  data = training,
                                  method = classifier,
                                  metric = "ROC",
                                  na.action = na.omit,
                                  trControl = train_control))
  } else {
    train_control <- trainControl(method = "cv",
                                  number = 5,
                                  selectionFunction = selection_rule,
                                  savePredictions = TRUE)
    capture.output(model <- train(train_formula,
                                  data = training,
                                  method = classifier,
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
                                selection_rule, sampling_method) {
  
  # Generates model and logs stats to file
  model <- generate_model(training, classifier, target_feature, log_file,
                          selection_rule, sampling_method)
  sink_f(model, log_file)
  
  # Saves model to file
  model_file <- file.path(output_folder, 
                          paste(classifier, "_model.rds", sep = ""))
  saveRDS(model, file = model_file)
  
  # Generates predictions on testing data and writes confusion matrix to file
  predictions <- predict(model, testing)
  cat_f("\n", log_file)
  sink_f(postResample(predictions, testing[[target_feature]]), log_file)
  if (nlevels(training[[target_feature]]) == 2) {
    sens <- sensitivity(predictions, testing[[target_feature]], positive = "rMAE")
    spec <- specificity(predictions, testing[[target_feature]], positive = "rMAE")
    cat_f(paste("sensitivity:", sens, "\n"), log_file)
    cat_f(paste("specificity:", spec, "\n"), log_file)
    cMat <- as.table(confusionMatrix(predictions, testing[[target_feature]]),
                     positive = "rMAE")
    matrix_file <- file.path(output_folder, paste(classifier, "matrix", sep = "_"))
    write.table(cMat, file = matrix_file, quote = FALSE)
  }
  
  # Writes predictions to file
  testing$prediction <- predictions
  testing$name <- testing_gene_names
  testing_file <- file.path(output_folder, paste(classifier, "_testing.tsv", sep = ""))
  write.table(testing, file = testing_file, sep = "\t", 
              quote = FALSE, row.names = FALSE, col.names = TRUE)
}

# Runs machine learning analyses on data
scores_ml <- function(scores_file, target_feature, classifier, output_folder,
                      selection_rule, sampling_method = "none", cores = 4) {
  
  # Checks that input file exists and creates output folder if it doesn't exist
  if (!file.exists(scores_file)) { stop("scores do not exist") }
  if (!dir.exists(output_folder)) { dir.create(output_folder) }
  
  # Creates summary file with header
  summary_file <- file.path(output_folder, 
                            paste(classifier, "summary.txt", sep = "_"))
  cat_f(paste(classifier, "summary\n\n"), summary_file, FALSE)
  
  # Enables multicore processing using doMC package
  registerDoMC(cores = cores)
  
  # Loads scores file into a data frame and removes unnecesary cols
  df <- read.csv(scores_file, sep = "\t")
  
  # Splits data into training and testing sets
  partition <- createDataPartition(df$status, times = 1, p = 0.8, list = FALSE)
  training <- df[partition,]
  testing <- df[-partition,]
  
  # Removes unnecessary cols from both sets, saving testing gene names
  testing_gene_names <- testing$name
  training <- training[, !(names(training) %in% c("start", "end", "chrom", "name"))]
  testing <- testing[, !(names(testing) %in% c("start", "end", "chrom", "name"))]
  
  # Writes training and testing dataframes to output folder
  write.table(testing, file = file.path(output_folder, "testing_set.csv"),
              sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
  
  # Generates model and saves it to output folder
  generate_classifier(training, testing, testing_gene_names, classifier, 
                      target_feature, output_folder, summary_file, 
                      selection_rule, sampling_method)
}