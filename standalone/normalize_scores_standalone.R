#!/usr/bin/env Rscript

##################
# LIBRARIES
##################


# Loads or installs all required packages
load_libraries <- function() {
  get_package("optparse")
  get_package("dplyr")
}

##################
# UTILITY FUNCTIONS
##################


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

#################
# MAIN FUNCTIONS
#################


# Normalizes the scores output from bigwig_to_scores based on
# either a control bigwig file or the length of each interval.
normalize_scores <- function(scores_file, output_file, 
                             control_file = NULL, drop_percent = 0.05) {
  
  # Opens scores file and converts NA values to 0
  scores <- read.csv(scores_file, sep = "\t", header = TRUE)
  scores[is.na(scores)] <- 0
  
  # Divides scores by control after dropping lowest percentile
  if (!is.null(control_file)) {
    
    # Reads in control file, converts NA values to 0 and generates percentiles
    control <- read.csv(control_file, sep = "\t", header = TRUE)
    control[is.na(control)] <- 0
    control <- control %>% mutate(percentile = rank(sum) / length(sum))
    
    # Removes indices in bottom 5th percentile from both data frames
    indices_to_keep <- which(control$percentile > drop_percent)
    control <- control[indices_to_keep,]
    scores <- scores[indices_to_keep,]
    
    # Divides scores sum by control sum
    scores$norm_sum <- scores$sum / control$sum
    
    # Or sets norm sum equal to mean column output from bwtool
  } else {
    scores$norm_sum <- scores$mean
  }
  
  # Generates percentile rank for scores based on normalized sum
  scores <- scores %>% mutate(percentile = rank(norm_sum) / length(norm_sum))
  
  # Sorts output dataframe by chromosome and gene name
  scores <- scores %>% group_by(chrom, name)
  
  # Writes new scores out to same file
  write.table(scores, file = output_file, sep = "\t", quote = FALSE,
              row.names = FALSE, col.names = TRUE)
}

##################
# COMMAND LINE INTERFACE
##################


# Loads required libraries
load_libraries()

# Builds option list using optparse
options = list(
  make_option(c("-i", "--scores_file"), type="character", default=NULL, 
              help="scores txt file output from bigwig_to_scores"),
  make_option(c("-o", "--output_file"), type="character", default=NULL, 
              help="normalized scores file to output"),
  make_option(c("-c", "--control_file"), type="character", default=NULL, 
              help="baseline input file [optional]"),
  make_option(c("-d", "--drop_percent"), type="double", default=0.05, 
              help="bottom enrichment percentile of genes to drop [default= %default]")
)

# Gets options and checks arguments
opt <- parse_args(OptionParser(option_list = options))
if (!file.exists(opt$scores_file)) { stop("score file does not exist") }
if (!(is.null(opt$control_file)) && !(file.exists(opt$scores_file))) { 
  stop("control file does not exist") 
}

# Extracts variables from args and passes them to normalize_scores
scores_file <- opt$scores_file
output_file <- opt$output_file
control_file <- opt$control_file
drop_percent <- opt$drop_percent
normalize_scores(scores_file, output_file, control_file, drop_percent)