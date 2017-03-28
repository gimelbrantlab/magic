#!/usr/bin/env Rscript

# Process bigwig files into scores files, for use in 
# generate.R and analyze.R

######
# MAIN FUNCTIONS
######


# Returns a given path's file extension after converting to lowercase
file_extension <- function(file_name) {
  components <- strsplit(file_name, "\\.")[[1]]
  return(tolower(components[length(components)]))
}

# Checks if a file exists as either an absolute or relative path
# without setting the working directory
file_check <- function(file_name, relative_dir) {
  relative_path <- file.path(relative_dir, file_name)
  if(file.exists(file_name) || file.exists(relative_path)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Changes a path to a relative pathname if it exists and the absolute
# pathname doesn't. Should only be called after file_check
file_to_relative <- function(file_name, relative_dir) {
  relative_path <- file.path(relative_dir, file_name)
  if (!file.exists(file_name) && file.exists(relative_path)) { 
    return(relative_path)
  } else {
    return(file_name)
  }
}

# Creates an input dataframe from a file with bigwig file paths
create_input_df <- function(file_name, output_folder) {
  
  # Reads file line by line
  lines <- readLines(file_name)
  
  # Instantiates lists to build up
  marks <- vector(length = length(lines))
  mark_files <- vector(length = length(lines))
  input_files <- vector(length = length(lines))
  
  # Counts number of marks added
  i = 1
  for (line in lines) {
    
    # Ignores commented lines
    if (startsWith(line, "#")) { next }
    
    # Splits line by tabs and ignores lines with too few arguments
    line <- strsplit(line, "\t")
    if (length(line[[1]]) < 2) { next }
    
    # Gets mark, mark file and input file if the latter is given
    mark <- line[[1]][1]
    mark_file <- line[[1]][2]
    input_file <- ""
    if (length(line[[1]]) >= 3) { input_file <- line[[1]][3] }
    
    # Adds mark to list, throws error if already added
    if (!(mark %in% marks)) { 
      marks[i] <- mark
    } else {
      stop("marks must have unique names")
    }
    
    # Adds input and control files to lists
    mark_files[i] <- mark_file
    input_files[i] <- input_file
    
    # Iterates counter
    i = i + 1
  }
  
  # Subsets vectors to exclude ignored lines
  to_remove <- c(i:length(lines))
  marks <- marks[-to_remove]
  mark_files <- mark_files[-to_remove]
  input_files <- input_files[-to_remove]
  
  # Checks if all files are valid pathnames, throws error if not
  # or if conflicting 
  mark_file_check <- lapply(mark_files, file_check, relative_dir = dirname(file_name))
  input_file_check <- lapply(input_files, file_check, relative_dir = dirname(file_name))
  if (FALSE %in% mark_file_check) { stop("not all given mark files exist") }
  if (FALSE %in% input_file_check) { stop("not all given input files exist") }
  
  # Changes absolute to relative pathnames if necessary
  mark_files <- lapply(mark_files, file_to_relative, relative_dir = dirname(file_name))
  input_files <- lapply(input_files, file_to_relative, relative_dir = dirname(file_name))
  
  # Changes empty strings to NAs
  input_files[input_files == ""] <- NA
  
  # Creates processed body and promoter filenames by mark
  processed_body <- paste(marks, "_norm_body.txt", sep = "")
  processed_body <- file.path(output_folder, processed_body)
  processed_promoter <- paste(marks, "_norm_promoter.txt", sep = "")
  processed_promoter <- file.path(output_folder, processed_promoter)
  
  # Builds dataframe, sorted by mark
  df <- data.frame(marks = marks, mark_files = mark_files, 
                   input_files = input_files, processed_body = processed_body,
                   processed_promoter = processed_promoter,
                   stringsAsFactors = FALSE)
  
  # Returns dataframe
  return(df)
}

# Runs input and control files, if applicable, through bigwig_to_scores
# and normalize_scores.R. Outputs processed text files in output_folder
process_input <- function(x, refseq_file, imprinted_file, 
                          bwtool_folder, output_folder,
                          dropped_file, promoter_length, 
                          drop_percent, drop_abs, 
                          overlap, filter_olf, 
                          filter_chroms, filter_imprinted) {
  
  # Gets mark and files from row in data frame
  mark <- x[[1]]
  mark_file <- x[[2]]
  input_file <- x[[3]]
  
  # Builds variables to pass to R scripts
  output_mark_body_file <- file.path(output_folder, paste(mark, "_mark_body.txt", sep = ""))
  output_mark_promoter_file <- file.path(output_folder, paste(mark, "_mark_promoter.txt", sep = ""))
  output_input_body_file <- file.path(output_folder, paste(mark, "_input_body.txt", sep = ""))
  output_input_promoter_file <- file.path(output_folder, paste(mark, "_input_promoter.txt", sep = ""))
  output_mark_body_norm_file <- file.path(output_folder, paste(mark, "_norm_body.txt", sep = ""))
  output_mark_promoter_norm_file <- file.path(output_folder, paste(mark, "_norm_promoter.txt", sep = ""))
  
  # Runs bigwig_to_scores.R on both mark and control files
  bigwig_to_scores(refseq_file, mark_file, imprinted_file,
                   bwtool_folder, output_mark_body_file,
                   output_mark_promoter_file, promoter_length,
                   overlap, filter_olf,
                   filter_chroms, filter_imprinted)
  if (!is.na(input_file)) {

    # Copies input if processed earlier
    # NEED ACTUAL LOGIC HERE TO MARK EARLIER PROCESSED FILE
    #old_process_file <- file.path(output_folder, paste(mark, "_input_body.txt", sep = ""))
    #if(!file.exists(output_control_body_file)) {
      bigwig_to_scores(refseq_file, input_file, imprinted_file,
                       bwtool_folder, output_input_body_file,
                       output_input_promoter_file, promoter_length,
                       overlap, filter_olf,
                       filter_chroms, filter_imprinted)
    # } else {
    #   file.copy(output_control_body_file)
    # }
  }
  
  # Runs normalize_scores.R
  if (!is.na(input_file)) {
    normalize_scores(output_mark_body_file, output_mark_body_norm_file, 
                     dropped_file, mark, "body",
                     output_input_body_file, drop_percent, drop_abs)
    if (promoter_length > 0) {
      normalize_scores(output_mark_promoter_file, output_mark_promoter_norm_file, 
                       dropped_file, mark, "promoter",
                       output_input_promoter_file, drop_percent, drop_abs) 
    }
  } else {
    normalize_scores(output_mark_body_file, output_mark_body_norm_file, 
                     dropped_file, mark, "body",
                     NA, drop_percent, drop_abs)
    if (promoter_length > 0) {
      normalize_scores(output_mark_promoter_file, output_mark_promoter_norm_file, 
                       dropped_file, mark, "promoter",
                       NA, drop_percent, drop_abs)
    }
  }
}

# Deletes intermediate output files from output folder
clean_intermediate <- function(output_folder, input_df, promoter_length) {
  
  # Generates lists of body and promoter output files
  input_df$mark_body <- file.path(output_folder, paste(input_df$marks, "_mark_body.txt", sep = ""))
  input_df$mark_promoter <- file.path(output_folder, paste(input_df$marks, "_mark_promoter.txt", sep = ""))
  input_df$control_body <- file.path(output_folder, paste(input_df$marks, "_input_body.txt", sep = ""))
  input_df$control_promoter <- file.path(output_folder, paste(input_df$marks, "_input_promoter.txt", sep = ""))
  
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
process_main <- function(current_folder, input_file, output_folder,
                         promoter_length, drop_percent, drop_abs, 
                         clean, overlap, refseq_file,
                         bed_file, filter_olf, filter_chroms, 
                         filter_imprinted, cores) {
  
  # Loads required scripts and libraries
  load_process_libraries()
  load_process_scripts(current_folder)
  
  # Checks that a reasonable number of cores was specified
  if ((cores < 0) || (cores >= detectCores())) { stop("invalid number of cores given") }
  
  # Parses input folder into dataframe with marks and files after
  # converting input file to absolute pathname
  input_file <- normalizePath(input_file)
  input_df <- create_input_df(input_file, output_folder)
  
  # Gets reference, output files and scripts based on position in program directory
  reference_folder <- file.path(dirname(current_folder), "reference")
  bin_folder <- file.path(dirname(current_folder), "bin")
  imprinted_file <- file.path(reference_folder, "imprinted_genes.tsv")
  bwtool_folder <- file.path(bin_folder, "bwtool")
  dropped_file <- file.path(output_folder, "dropped_genes.tsv")
  
  # Resets dropped genes file
  header <- paste("Genes dropped due to baseline enrichment beneath the ",
                  drop_percent, " percentile and below", drop_abs, "mean value", sep = "")
  cat_f(header, dropped_file, FALSE)
  cat_f("gene_name\tlow_baseline_mark", dropped_file)
  
  # Gets refseq file from either references folder or user input
  if (is.null(bed_file)) {
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
    
  # If bed file is given, passes custom bed file to refseq file variable
  # and ignores the given refseq argument
  } else {
    if (file.exists(bed_file)) {
      if (file_extension(bed_file) != "bed") {
        stop("bed file does not have the file extension .bed")
      }
      refseq_file <- bed_file
    } else {
      stop("bed file does not exist - use refseq option unless using custom bed file")
    }
  }
  
  # Applies first two steps of pipeline to each row of input_df in parallel
  #before_time <- Sys.time()
  cluster <- makeCluster(cores, type = "FORK")
  parApply(cluster, input_df, 1, process_input,
        refseq_file = refseq_file, imprinted_file = imprinted_file,
        bwtool_folder = bwtool_folder, output_folder = output_folder,
        dropped_file = dropped_file, promoter_length = promoter_length,
        drop_percent = drop_percent, drop_abs = drop_abs,
        overlap = overlap, filter_olf = filter_olf,
        filter_chroms = filter_chroms, filter_imprinted = filter_imprinted)
  stopCluster(cluster)
  #print(Sys.time() - before_time)

  # Joins all files into two tables with percentile scores or normalized sum
  norm_output_file <- file.path(output_folder, "joined_scores_norm.txt")
  percentile_output_file <- file.path(output_folder, "joined_scores_percentile.txt")
  join_input_main(input_df$processed_body, input_df$processed_promoter, input_df$marks,
                  percentile_output_file, norm_output_file, promoter_length)
  
  # Removes intermediate files from output folder if specified
  if (clean) {
    clean_intermediate(output_folder, input_df, promoter_length)
  }
  
  cat("Data processing complete.\n")
}

######
# COMMAND LINE INTERFACE
######


# Suppressingfire's SO solution to get executing script.
# Found at http://stackoverflow.com/questions/30468412/dplyr-join-warning-joining-factors-with-different-levels
args <- commandArgs()
current_folder <- dirname(sub("--file=", "", args[grep("--file=", args)]))

# Gets path to models folder and loads utility functions
models_folder <- file.path(current_folder, "..", "models")
source(file.path(current_folder, "utils.R"))

# Loads optparse
load_initial_libraries()

# Builds option list using optparse
options = list(
  make_option(c("-i", "--input_file"), type="character", default=NULL, 
              help="input file, see readme or included example file for description"),
  make_option(c("-o", "--output_folder"), type="character", default="output", 
              help="output folder [default= %default]"),
  make_option(c("-p", "--promoter_length"), type="integer", default=5000, 
              help="upstream promoter region length [default= %default]"),
  make_option(c("-d", "--drop_percent"), type="double", default=0.01, 
              help="bottom enrichment percentile of genes to drop [default= %default]"),
  make_option(c("-a", "--drop_absolute"), type="double", default=0.0,
              help="bottom absolute mean value of genes to drop [default=%default]"),
  make_option(c("-e", "--no_clean_intermediate"), action="store_false", default=TRUE, 
              help="leave intermediate files [default= %default]"),
  make_option(c("-l", "--no_overlap"), action="store_false", default=TRUE, 
              help="remove promoter overlap with gene body, see readme for description [default= %default]"),
  make_option(c("-r", "--refseq_file"), type="character", default=NULL, 
              help="refseq file, see readme for description"),
  make_option(c("-b", "--bed_file"), type="character", default=NULL, 
              help="custom bed file to use in place of refseq"),
  make_option(c("-f", "--no_filter_olf"), action="store_false", default=TRUE, 
              help="disable olfactory receptor gene filtering [default= %default]"),
  make_option(c("-c", "--no_filter_chroms"), action="store_false", default=TRUE, 
              help="disable extra, partially-assembled and sex-chromosome gene filtering [default= %default]"),
  make_option(c("-m", "--no_filter_imprinted"), action="store_false", default=TRUE, 
              help="disable imprinted gene filtering [default= %default]"),
  make_option(c("-s", "--cores"), type="integer", default=1, 
              help="number of cores to use for data processing [default= %default]"),
  make_option(c("-q", "--quiet"), action="store_true", default=FALSE, 
              help="disables console output [default= %default]")
)

# Gets options and checks arguments
opt <- parse_args(OptionParser(option_list = options))
if (!file.exists(opt$input_file)) { stop("input file does not exist") }
if (!dir.exists(opt$output_folder)) { dir.create(opt$output_folder, recursive = TRUE) }

# Extracts variables from args
input_file <- opt$input_file
output_folder <- opt$output_folder
promoter_length <- opt$promoter_length
drop_percent <- opt$drop_percent
drop_abs <- opt$drop_absolute
clean <- opt$no_clean_intermediate
overlap <- opt$no_overlap
refseq_file <- opt$refseq_file
bed_file <- opt$bed_file
filter_olf <- opt$no_filter_olf
filter_chroms <- opt$no_filter_chroms
filter_imprinted <- opt$no_filter_imprinted
cores <- opt$cores
quiet <- opt$quiet

# Calls main function, disabling output if running in quiet mode
if (!quiet) {
  invisible(process_main(current_folder, input_file, output_folder,
                         promoter_length, drop_percent, drop_abs, 
                         clean, overlap, refseq_file,
                         bed_file, filter_olf, filter_chroms, 
                         filter_imprinted, cores))
} else {
  process_main(current_folder, input_file, output_folder,
               promoter_length, drop_percent, drop_abs, 
               clean, overlap, refseq_file,
               bed_file, filter_olf, filter_chroms, 
               filter_imprinted, cores)
}