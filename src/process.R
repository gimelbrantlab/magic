#!/usr/bin/env Rscript

# Process bigwig files into scores files, for use in 
# generate.R and analyze.R

######
# MAIN FUNCTIONS
######


# Grabs all input files from input folder. Input must be named as
# "X_input.bigWig" or "X_input.bw" whereas control files, optional,
# must be labeled as "X_control." Returns dataframe with marks
# associated with input and control files
create_input_df <- function(input_folder, output_folder) {
  
  # Lists to build up
  input_files <- c()
  control_files <- c()
  marks <- c()
  
  # Appends matching files in directory to above lists
  all_files <- list.files(input_folder, recursive = FALSE)
  for (file in all_files) {
    
    if (grepl("*_mark.bigWig", file, ignore.case = TRUE) | 
        grepl("*_mark.bw", file, ignore.case = TRUE)) {
      mark <- strsplit(file, "_")[[1]][1]
      if (!(mark %in% marks)) { 
        marks <- c(marks, mark) 
      }
      input_files <- c(input_files, file)
    } 
    
    else if (grepl("*_control.bigWig", file, ignore.case = TRUE) | 
             grepl("*_control.bw", file, ignore.case = TRUE)) {
      mark <- strsplit(file, "_")[[1]][1]
      if (!(mark %in% marks)) { 
        marks <- c(marks, mark) 
      }
      control_files <- c(control_files, file)
    }
  }
  
  # Builds dataframe, sorted by mark
  df <- data.frame(mark = character(), input = character(), 
                   control = character(), processed_body = character(),
                   processed_promoter = character(),
                   stringsAsFactors = FALSE)
  for (mark in marks) {
    input_pos <- grepl(paste(mark, "_", sep = ""), input_files)
    control_pos <- grepl(paste(mark, "_", sep = ""), control_files)
    
    # Gets files if they correspond to a mark
    input <- NA
    if (any(input_pos)) { 
      input <- input_files[input_pos]
    }
    control <- NA
    if (any(control_pos)) { 
      control <- control_files[control_pos]
    }
    
    # Adds promoter region and normalized scores files, created later, to row
    mark <- tolower(mark)
    processed_body <- paste(mark, "_norm_body.txt", sep = "")
    processed_promoter <- paste(mark, "_norm_promoter.txt", sep = "")
    
    # Appends folder to each file
    input <- file.path(input_folder, input)
    processed_body <- file.path(output_folder, processed_body)
    processed_promoter <- file.path(output_folder, processed_promoter)
    if(!is.na(control)) { 
      control <- file.path(input_folder, control)
    }
    
    # Appends new row with mark and files to dataframe
    row <- data.frame(mark = mark, input = input, control = control, 
                      processed_body = processed_body,
                      processed_promoter = processed_promoter,
                      stringsAsFactors = FALSE)
    df <- bind_rows(df, row)
  }
  
  # Returns dataframe
  return(df)
}

# Runs input and control files, if applicable, through bigwig_to_scores
# and normalize_scores.R. Outputs processed text files in output_folder
process_input <- function(x, input_folder, refseq_file, imprinted_file, 
                          bwtool_folder, output_folder,
                          dropped_file, filter_input, 
                          promoter_length, drop_percent, 
                          overlap) {
  
  # Gets mark and files from row in data frame
  mark <- x[[1]]
  mark_file <- x[[2]]
  control_file <- x[[3]]
  
  # Builds variables to pass to R scripts
  output_mark_body_file <- file.path(output_folder, paste(mark, "_mark_body.txt", sep = ""))
  output_mark_promoter_file <- file.path(output_folder, paste(mark, "_mark_promoter.txt", sep = ""))
  output_control_body_file <- file.path(output_folder, paste(mark, "_control_body.txt", sep = ""))
  output_control_promoter_file <- file.path(output_folder, paste(mark, "_control_promoter.txt", sep = ""))
  output_mark_body_norm_file <- file.path(output_folder, paste(mark, "_norm_body.txt", sep = ""))
  output_mark_promoter_norm_file <- file.path(output_folder, paste(mark, "_norm_promoter.txt", sep = ""))
  
  # Runs bigwig_to_scores.R on both mark and control files
  bigwig_to_scores(refseq_file, mark_file, imprinted_file,
                   bwtool_folder, output_mark_body_file,
                   output_mark_promoter_file, filter_input,
                   overlap, promoter_length)
  if (!is.na(x[[3]])) {
    bigwig_to_scores(refseq_file, control_file, imprinted_file,
                     bwtool_folder, output_control_body_file,
                     output_control_promoter_file, filter_input,
                     overlap, promoter_length)
  }
  
  # Runs normalize_scores.R
  if (!is.na(x[[3]])) {
    normalize_scores(output_mark_body_file, output_mark_body_norm_file, 
                     dropped_file, mark, "body",
                     output_control_body_file, drop_percent)
    normalize_scores(output_mark_promoter_file, output_mark_promoter_norm_file, 
                     dropped_file, mark, "promoter",
                     output_control_promoter_file, drop_percent) 
  } else {
    normalize_scores(output_mark_body_file, output_mark_body_norm_file, 
                     dropped_file, mark, "body",
                     NA, drop_percent)
    normalize_scores(output_mark_promoter_file, output_mark_promoter_norm_file, 
                     dropped_file, mark, "promoter",
                     NA, drop_percent)
  }
}

# Deletes intermediate output files from output folder
clean_intermediate <- function(output_folder, input_df, promoter_length) {
  
  # Generates lists of body and promoter output files
  input_df$mark_body <- file.path(output_folder, paste(input_df$mark, "_mark_body.txt", sep = ""))
  input_df$mark_promoter <- file.path(output_folder, paste(input_df$mark, "_mark_promoter.txt", sep = ""))
  input_df$control_body <- file.path(output_folder, paste(input_df$mark, "_control_body.txt", sep = ""))
  input_df$control_promoter <- file.path(output_folder, paste(input_df$mark, "_control_promoter.txt", sep = ""))
  
  # Removes all intermediate files
  all_files <- list.files(output_folder, recursive = FALSE, full.names = TRUE)
  for (file in all_files) {
    
    # Removes refseq file (loci passed to bwtool) and body output files
    if (file == file.path(output_folder, "loci.bed")) {
      file.remove(file)
    } else if (file %in% input_df$mark_body) {
      file.remove(file)
    } else if (file %in% input_df$control_body) {
      file.remove(file)
    } else if (file %in% input_df$processed_body) {
      file.remove(file)
    }
    
    # Removes promoter files if created
    if (promoter_length > 0) {
      if (file %in% input_df$mark_promoter) {
        file.remove(file)
      } else if (file %in% input_df$control_promoter) {
        file.remove(file)
      } else if (file %in% input_df$processed_promoter) {
        file.remove(file)
      } 
    }
  }
}

# Processes a given bigwig file
process_main <- function(current_folder, input_folder, output_folder,
                         filter_input, promoter_length, drop_percent,
                         clean, overlap, refseq_file, 
                         training_genes_file) {
  
  # Loads required scripts and libraries
  load_process_libraries()
  load_process_scripts(current_folder)
  
  # Parses input folder into dataframe with marks and files
  input_df <- create_input_df(input_folder, output_folder)
  
  # Gets reference, output files and scripts based on position in program directory
  reference_folder <- file.path(dirname(current_folder), "reference")
  bin_folder <- file.path(dirname(current_folder), "bin")
  imprinted_file <- file.path(reference_folder, "imprinted_genes.tsv")
  bwtool_folder <- file.path(bin_folder, "bwtool")
  dropped_file <- file.path(output_folder, "dropped_genes.tsv")
  
  # Resets dropped genes file
  header <- paste("Genes dropped due to baseline enrichment beneath the ",
                  drop_percent, " percentile", sep = "")
  cat_f(header, dropped_file, FALSE)
  cat_f("gene_name\tlow_baseline_mark", dropped_file)
  
  # Gets correct training genes file depending on species
  if (training_genes_file == "mouse") {
    training_genes_file <- file.path(reference_folder, "mouse_tg.tsv")
  } else if (training_genes_file == "human") {
    training_genes_file <- file.path(reference_folder, "human_tg.tsv")
  } else {
    training_genes_file <- file.path(reference_folder, training_genes_file)
    if (!file.exists(training_genes_file)) {
      stop("training genes file does not exist (use 'mouse' or 'human')")
    }
  }
  
  # Gets refseq file from either references folder or user input
  if (!file.exists(refseq_file)) {
    possible_file_1 <- file.path(reference_folder, tolower(refseq_file))
    possible_file_2 <- file.path(reference_folder, 
                                 tolower(paste(refseq_file, "refseq.txt", sep = "_")))
    if (file.exists(possible_file_1)) {
      refseq_file <- possible_file_1
    } else if (file.exists(possible_file_2)) {
      refseq_file <- possible_file_2
    } else {
      stop("refseq file does not exist (check readme for possible options)")
    }
  }
  
  # Applies first two steps of pipeline to each row of input_df
  apply(input_df, 1, process_input,
        input_folder = input_folder, refseq_file = refseq_file,
        imprinted_file = imprinted_file, bwtool_folder = bwtool_folder,
        output_folder = output_folder, dropped_file = dropped_file,
        filter_input = filter_input, promoter_length = promoter_length,
        drop_percent = drop_percent, overlap = overlap)

  # Joins all files into two tables with percentile scores or normalized sum
  norm_output_file <- file.path(output_folder, "joined_scores_norm.txt")
  percentile_output_file <- file.path(output_folder, "joined_scores_percentile.txt")
  join_input_main(input_df$processed_body, input_df$processed_promoter, input_df$mark,
                  training_genes_file, percentile_output_file, norm_output_file,
                  promoter_length)
  
  # Removes intermediate files from output folder if specified
  if (clean) {
    clean_intermediate(output_folder, input_df, promoter_length)
  }
  
  cat("Data processing complete.")
}

######
# COMMAND LINE INTERFACE
######


# Loads optparse
load_initial_libraries()

# Suppressingfire's SO solution to get executing script.
# Found at http://stackoverflow.com/questions/30468412/dplyr-join-warning-joining-factors-with-different-levels
args <- commandArgs()
current_folder <- dirname(sub("--file=", "", args[grep("--file=", args)]))

# Builds option list using optparse
options = list(
  make_option(c("-i", "--input_folder"), type="character", default=NULL, 
              help="input folder, see readme for description"),
  make_option(c("-o", "--output_folder"), type="character", default="output", 
              help="output folder [default= %default]"),
  make_option(c("-f", "--no_filter"), action="store_false", default=TRUE, 
              help="disable sex, extra chrom and gene filtering [default= %default]"),
  make_option(c("-p", "--promoter_length"), type="integer", default=5000, 
              help="upstream promoter region length [default= %default]"),
  make_option(c("-d", "--drop_percent"), type="double", default=0.01, 
              help="bottom enrichment percentile of genes to drop [default= %default]"),
  make_option(c("-c", "--no_clean_intermediate"), action="store_false", default=TRUE, 
              help="leave intermediate files [default= %default]"),
  make_option(c("-l", "--no_overlap"), action="store_false", default=TRUE, 
              help="remove promoter overlap with gene body, see readme for description [default= %default]"),
  make_option(c("-r", "--refseq_file"), type="character", default=NULL, 
              help="refseq file, see readme for description"),
  make_option(c("-tr", "--training_genes_file"), type="character", default="mouse", 
              help="'mouse' or 'human' for training genes set to use [default= %default]"),
  make_option(c("-q", "--quiet"), action="store_true", default=FALSE, 
              help="disables console output [default= %default]")
)

# Gets options and checks arguments
opt <- parse_args(OptionParser(option_list = options))
if (!dir.exists(opt$input_folder)) { stop("input directory does not exist") }
if (!dir.exists(opt$output_folder)) { dir.create(opt$output_folder) }

# Extracts variables from args
input_folder <- opt$input_folder
output_folder <- opt$output_folder
filter_input <- opt$no_filter
promoter_length <- opt$promoter_length
drop_percent <- opt$drop_percent
clean <- opt$no_clean_intermediate
overlap <- opt$no_overlap
refseq_file <- opt$refseq_file
training_genes_file <- tolower(opt$training_genes_file)
quiet <- opt$quiet

# Calls main function, disabling output if running in quiet mode
if (!quiet) {
  invisible(process_main(current_folder, input_folder, output_folder,
                         filter_input, promoter_length, drop_percent,
                         clean, overlap, refseq_file, 
                         training_genes_file))
} else {
  process_main(current_folder, input_folder, output_folder,
               filter_input, promoter_length, drop_percent,
               clean, overlap, refseq_file, 
               raining_genes_file)
}