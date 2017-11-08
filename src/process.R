#!/usr/bin/env Rscript

# Processes bigwig files into scores files, for use in generate.R and analyze.R

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
  if (file_name == "") {
    return(file_name)
  }
  else if (!file.exists(file_name) && file.exists(relative_path)) { 
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
  line_num = 1
  for (line in lines) {
    
    # Ignores commented lines
    if (startsWith(line, "#")) {
      line_num = line_num + 1
      next 
    }
    
    # Splits line by tabs and ignores lines with too few arguments
    line <- strsplit(line, "\t")
    if (length(line[[1]]) < 2) { 
      if (length(line[[1]]) == 1) { 
        print(paste("ignoring line", line_num, "with too few arguments:", line[[1]]))
      }
      next
    }
    
    # Gets mark, mark file and input file if the latter is given
    mark <- line[[1]][1]
    mark_file <- line[[1]][2]
    input_file <- ""
    if (mark_file == "") {
      stop_msg <- paste("mark file on line", line_num, 
                        "missing, check that your input file has only one tab between arguments")
      stop(stop_msg)
    }
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
    
    # Iterates counters
    i = i + 1
    line_num = line_num + 1
  }
  
  # Subsets vectors to exclude ignored lines
  to_remove <- c(i:length(lines))
  marks <- marks[-to_remove]
  mark_files <- mark_files[-to_remove]
  input_files <- input_files[-to_remove]
  
  # Checks if all files are valid pathnames, throws error if not
  # or if conflicting 
  mark_file_check <- sapply(mark_files, USE.NAMES = FALSE, 
                            file_check, relative_dir = dirname(file_name))
  input_file_check <- sapply(input_files, USE.NAMES = FALSE,
                             file_check, relative_dir = dirname(file_name))
  if (FALSE %in% mark_file_check) { stop("not all given mark files exist") }
  if (FALSE %in% input_file_check) { stop("not all given input files exist") }
  
  # Changes absolute to relative pathnames if necessary
  mark_files <- sapply(mark_files, USE.NAMES = FALSE,
                       file_to_relative, relative_dir = dirname(file_name))
  input_files <- sapply(input_files, USE.NAMES = FALSE,
                        file_to_relative, relative_dir = dirname(file_name))
  
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
process_input <- function(x, body_bed_file, promoter_bed_file,
                          imprinted_file, bwtool_folder, 
                          output_folder, dropped_file,
                          drop_percent, drop_abs, 
                          filter_olf, filter_chroms, 
                          filter_imprinted) {
  
  # Gets mark and files from row in data frame
  mark <- x[[1]]
  mark_file <- x[[2]]
  input_file <- x[[3]]
  
  # Builds variables to pass to R scripts
  output_mark_body_file <- file.path(output_folder, paste(mark, "_mark_body.txt", sep = ""))
  output_input_body_file <- file.path(output_folder, paste(mark, "_input_body.txt", sep = ""))
  output_mark_body_norm_file <- file.path(output_folder, paste(mark, "_norm_body.txt", sep = ""))
  
  # Optionally sets promoter file paths if promoter length is specified
  output_mark_promoter_file <- NA
  output_input_promoter_file <- NA
  output_mark_promoter_norm_file <- NA
  if (!is.na(promoter_bed_file)) {
    output_mark_promoter_file <- file.path(output_folder, paste(mark, "_mark_promoter.txt", sep = ""))
    output_input_promoter_file <- file.path(output_folder, paste(mark, "_input_promoter.txt", sep = ""))
    output_mark_promoter_norm_file <- file.path(output_folder, paste(mark, "_norm_promoter.txt", sep = ""))
  }
  
  # Runs bigwig_to_scores.R on both mark and control files
  bigwig_to_scores(mark_file, imprinted_file, bwtool_folder, 
                   output_mark_body_file, body_bed_file,
                   output_mark_promoter_file, promoter_bed_file,
                   filter_olf, filter_chroms, 
                   filter_imprinted)
  if (!is.na(input_file)) {

    # Copies input if processed earlier
    # NEED ACTUAL LOGIC HERE TO MARK EARLIER PROCESSED FILE
    #old_process_file <- file.path(output_folder, paste(mark, "_input_body.txt", sep = ""))
    #if(!file.exists(output_control_body_file)) {
    
      bigwig_to_scores(input_file, imprinted_file, bwtool_folder, 
                       output_input_body_file, body_bed_file,
                       output_input_promoter_file, promoter_bed_file,
                       filter_olf, filter_chroms,
                       filter_imprinted)
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
    if (file == file.path(output_folder, "body_loci.bed")) {
      file.remove(file)
    } else if (file == file.path(output_folder, "promoter_loci.bed")) {
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

# Generates histograms of input value distributions
generate_histograms <- function(input_df, output_folder, promoter_length,
                                xmax = NA) {
  
  # Creates lists of processed input files
  output_input_body_files <- file.path(output_folder, 
                                       paste(input_df$marks, "_input_body.txt", sep = ""))
  output_input_promoter_files <- NA
  if (promoter_length > 0) {
    output_input_promoter_files <- file.path(output_folder, 
                                             paste(input_df$marks, "_input_promoter.txt", sep = ""))
  }
  
  # Gets number of rows of input files
  num_rows <- nrow(read.csv(output_input_body_files[1], sep ="\t", header = TRUE))
  
  # Creates dataframes of mean input values necessary for histogram creation.
  # Also finds the largest quintile across all marks for both gene regions
  mark <- NULL
  quintile <- NULL
  largest_body_quintile <- 0
  largest_promoter_quintile <- 0
  largest_body_max <- 0
  largest_promoter_max <- 0
  histogram_body_df <- data.frame(id = 1:num_rows)
  histogram_promoter_df <- data.frame(id = 1:num_rows)
  for (i in 1:length(input_df$input_files)) {
    if (!is.null(input_df$input_files[i])) {
      
      # Body region column creation and quintile calculation
      mark <- input_df$marks[i]
      body_df <- read.csv(output_input_body_files[i], sep = "\t", header = TRUE)
      histogram_body_df[[mark]] <- body_df$mean
      quintile <- quantile(histogram_body_df[[mark]], c(.8), na.rm = TRUE)[[1]]
      
      # Gets largest quintile and max value
      if (quintile > largest_body_quintile) {
        largest_body_quintile <- quintile
      }
      if (max(histogram_body_df[[mark]], na.rm = TRUE) > largest_body_max) {
        largest_body_max <- max(histogram_body_df[[mark]], na.rm = TRUE)
      }
      
      # Promoter region column creation and quintile calculation
      if (promoter_length > 0) {
        promoter_df <- read.csv(output_input_promoter_files[i], sep = "\t", header = TRUE)
        histogram_promoter_df[[mark]] <- promoter_df$mean
        quintile <- quantile(histogram_promoter_df[[mark]], c(.8), na.rm = TRUE)[[1]]
        
        # Gets largest quintile and max value
        if (quintile > largest_promoter_quintile) {
          largest_promoter_quintile <- quintile
        }
        if (max(histogram_promoter_df[[mark]], na.rm = TRUE) > largest_promoter_max) {
          largest_promoter_max <- max(histogram_promoter_df[[mark]], na.rm = TRUE)
        }
      }
    }
  }
  
  # Removes id columns, melts dataframes, and converts NAs to 0s
  histogram_body_df <- histogram_body_df[,-c(1)]
  histogram_body_df <- suppressMessages(melt(histogram_body_df))
  histogram_body_df$value[is.na(histogram_body_df$value)] = 0
  if (promoter_length > 0) {
    histogram_promoter_df <- histogram_promoter_df[,-c(1)]
    histogram_promoter_df <- suppressMessages(melt(histogram_promoter_df))
  }
  
  # Sets xmax for both gene body and promoter regions to largest quintile.
  # Ignores quintile values if the user manually sets an xmax value
  body_xmax <- NA
  promoter_xmax <- NA
  if (is.na(xmax)) {
    body_xmax <- largest_body_quintile
    promoter_xmax <- largest_promoter_quintile
  } else {
    body_xmax <- xmax
    promoter_xmax <- xmax
  }
  
  # Creates histograms of input data using facet_wrap if more than 1 mark
  if(length(input_df$input_files) > 1) {
    
    # Creates body histogram and saves to file
    body_hist <- ggplot(histogram_body_df, aes(x = value)) + 
      facet_wrap(~variable, scales = "free_x") + 
      geom_histogram(binwidth = 0.02, na.rm = TRUE) +
      xlim(0, body_xmax) +
      xlab("mean count per gene body of input")
    body_hist_file <- file.path(output_folder, "input_body_hist.png")
    ggsave(body_hist_file, width = 4, height = 4)
    
    # Creates promoter histogram and saves to file
    if (promoter_length > 0) {
      promoter_hist <- ggplot(histogram_promoter_df, aes(x = value)) + 
        facet_wrap(~variable, scales = "free_x") + 
        geom_histogram(binwidth = 0.02, na.rm = TRUE) +
        xlim(0, promoter_xmax) +
        xlab("mean count per gene promoter of input")
      promoter_hist_file <- file.path(output_folder, "input_promoter_hist.png")
      ggsave(promoter_hist_file, width = 4, height = 4)
    }
    
  # If there is only one mark, creates non-wrapped histograms of the input data
  } else {
    
    # Creates body histogram and saves to file
    body_hist <- ggplot(histogram_body_df, aes(x = value)) + 
      geom_histogram(binwidth = 0.02, na.rm = TRUE) +
      xlim(0, body_xmax) +
      xlab("mean count per gene body of input")
    body_hist_file <- file.path(output_folder, 
                                paste(mark, "input_body_hist.png", sep = "_"))
    ggsave(body_hist_file, width = 4, height = 4)
    
    # Creates promoter histogram and saves to file
    if (promoter_length > 0) {
      promoter_hist <- ggplot(histogram_promoter_df, aes(x = value)) + 
        geom_histogram(binwidth = 0.02, na.rm = TRUE) +
        xlim(0, promoter_xmax) +
        xlab("mean count per gene promoter of input")
      promoter_hist_file <- file.path(output_folder, 
                                      paste(mark, "input_promoter_hist.png", sep = "_"))
      ggsave(promoter_hist_file, width = 4, height = 4)
    }
  }
  
  # Tells the user how much data is displayed on the histograms
  if(is.na(xmax)) {
    cat(paste("Body and promoter histograms display data up to",
              body_xmax,
              "and", 
              promoter_xmax,
              "along the x-axis, respectively\n"))
  } else {
    cat(paste("All body and promoter histograms display data up to", 
              xmax, 
              "along the x-axis\n"))
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
  if (grepl("hg", refseq_file, fixed=TRUE) | grepl("human", refseq_file, fixed=TRUE)) { 
    imprinted_file <- file.path(reference_folder, "imprinted_genes_human.tsv")
  }
  else if (grepl("mm", refseq_file, fixed=TRUE) | grepl("mouse", refseq_file, fixed=TRUE)) { 
    imprinted_file <- file.path(reference_folder, "imprinted_genes_mouse.tsv")
  }
  else {
    if (!is.null(bed_file)) {
      if (grepl("hg", bed_file, fixed=TRUE) | grepl("human", bed_file, fixed=TRUE))  {
        imprinted_file <- file.path(reference_folder, "imprinted_genes_human.tsv")
      }
      else if (grepl("mm", bed_file, fixed=TRUE) | grepl("mouse", bed_file, fixed=TRUE) ) {
        imprinted_file <- file.path(reference_folder, "imprinted_genes_mouse.tsv")
      }
    }
    warning("Unknown genome, using human list of imprinted genes")
    imprinted_file <- file.path(reference_folder, "imprinted_genes_human.tsv")
  }
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
  
  # Writes the refseq file or bed file out to "loci.bed" for
  # the body and promoter regions
  body_bed_file <- file.path(output_folder, "body_loci.bed")
  promoter_bed_file <- file.path(output_folder, "promoter_loci.bed")
  make_bed(refseq_file, body_bed_file, 0, 
           overlap)
  if (promoter_length > 0) {
    make_bed(refseq_file, promoter_bed_file, promoter_length, 
             overlap)
  } else {
    promoter_bed_file <- NA
  }
  
  # Applies first two steps of pipeline to each row of input_df in parallel
  #before_time <- Sys.time()
  if (cores > 1) {
    cluster <- makeCluster(cores, type = "FORK")
    parApply(cluster, input_df, 1, process_input,
             body_bed_file = body_bed_file, promoter_bed_file = promoter_bed_file,
             imprinted_file = imprinted_file, bwtool_folder = bwtool_folder, 
             output_folder = output_folder, dropped_file = dropped_file, 
             drop_percent = drop_percent, drop_abs = drop_abs,
             filter_olf = filter_olf, filter_chroms = filter_chroms, 
             filter_imprinted = filter_imprinted)
    stopCluster(cluster)
  } else if (cores == 1) {
    apply(input_df, 1, process_input,
          body_bed_file = body_bed_file, promoter_bed_file = promoter_bed_file,
          imprinted_file = imprinted_file, bwtool_folder = bwtool_folder, 
          output_folder = output_folder, dropped_file = dropped_file, 
          drop_percent = drop_percent, drop_abs = drop_abs,
          filter_olf = filter_olf, filter_chroms = filter_chroms, 
          filter_imprinted = filter_imprinted)
  } else {
    stop("must specify >= 1 core, e.g. with argument '-s 2'")
  }
  #print(Sys.time() - before_time)

  # Joins all files into two tables with percentile scores or normalized sum
  norm_output_file <- file.path(output_folder, "joined_scores_norm.txt")
  percentile_output_file <- file.path(output_folder, "joined_scores_percentile.txt")
  join_input_main(input_df$processed_body, input_df$processed_promoter, input_df$marks,
                  percentile_output_file, norm_output_file, promoter_length)
  
  # Only generates histograms if input files exist
  if(!all(is.na(input_df$input_files))) {
    generate_histograms(input_df, output_folder, promoter_length)
  }
  
  # Removes intermediate files from output folder if specified
  if (clean) {
    clean_intermediate(output_folder, input_df, promoter_length)
  }
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
  make_option(c("-p", "--promoter_length"), type="integer", default=2500, 
              help="upstream promoter region length [default= %default]"),
  make_option(c("-d", "--drop_percent"), type="double", default=0.01, 
              help="bottom enrichment percentile of genes to drop [default= %default]"),
  make_option(c("-a", "--drop_absolute"), type="double", default=1.0,
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
