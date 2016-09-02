#!/usr/bin/env Rscript

# Standalone version of script, to be run from the command line as
# opposed to being called from predict_mae.R

##################
# LIBRARIES
##################


# Loads or installs all required packages
load_libraries <- function() {
  get_package("optparse")
  get_package("reshape2")
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



# Appends gene names to regions in the counts file, and removes sex-linked 
# and olfactory receptor genes.
filter_counts <- function(counts_file, bed_file, imprinted_file) {
  
  # Opens files and adds meaningful colnames
  counts <- read.csv(counts_file, sep = "\t")
  bed <- read.csv(bed_file, sep = "\t", header = FALSE)
  imprinted <- read.csv(imprinted_file, sep = "\t")
  colnames(counts)[1] <- "chrom"
  colnames(bed)[1] <- "chrom"
  colnames(bed)[2] <- "chromStart"
  colnames(bed)[3] <- "chromEnd"
  colnames(bed)[4] <- "name"
  
  # Checks to make sure counts are the same length as loci
  if (length(counts[,1]) != length(bed[,1])) {
    stop("number of counts not equal to number of loci")
  }
  
  # Appends gene names to counts
  counts$name <- bed$name
  
  # Removes imprinted and olfactory receptor genes
  counts <- counts %>% filter(!(name %in% imprinted[,1]))
  counts <- counts %>% filter(!(grepl('^Olfr', name)))
  
  # List of chromosomes to keep
  chrom_list <- c("chr1", "chr2", "chr3", "chr4", "chr5",
                  "chr6", "chr7", "chr8", "chr9", "chr10",
                  "chr11", "chr12", "chr13", "chr14", "chr15",
                  "chr16", "chr17", "chr18", "chr19")
  
  # Removes sex-linked, partially assembled and extra chromosomes
  counts <- counts %>% filter(chrom %in% chrom_list)
  
  # Explicitly returns filtered counts
  return(counts)
}

# Calls bwtool's summarize function on input bigwig file
# and refseq bed file to return file with counts
bw_to_counts <- function(bed_file, bw_file, bwtool_folder, output_file) {
  
  # Builds command to run bwtool and executes it
  bwtool_file <- file.path(bwtool_folder, "bwtool")
  args <- paste("summary", bed_file, bw_file, output_file, 
                "-header", "-with-sum")
  try(system2(bwtool_file, args))
}

# Reads refseq table, calculates promoter region upstream of
# TSS, and culls all but the longest transcript for a gene.
# Outputs a bed file
refseq_to_bed <- function(refseq_file, promoter_length) {
  
  # Reads refseq table into dataframe
  refseq <- read.csv(refseq_file, sep = "\t")
  colnames(refseq)[1] <- "bin"
  
  # Calculates each transcript length
  refseq["transcriptLength"] <- abs(refseq["txEnd"] - refseq["txStart"])
  
  # Removes all but the longest transcript for each gene and chromosome
  refseq <- refseq %>% group_by(name2, chrom) %>% 
    slice(which.max(transcriptLength))
  
  # Adds one to each transcript position, to move from refseq
  # 0-based indexing to bigwig 1-based indexing
  refseq$txStart <- refseq$txStart + 1
  refseq$txEnd <- refseq$txEnd + 1
  
  # Separates promoter regions if specified
  if (promoter_length > 0) {
    i <- refseq$strand == "+"
    refseq$txEnd[i] <- refseq$txStart[i] + promoter_length
    refseq$txStart[i] <- refseq$txStart[i] - promoter_length
    refseq$txStart[!i] <- refseq$txEnd[!i] - promoter_length
    refseq$txEnd[!i] <- refseq$txEnd[!i] + promoter_length
  }
  
  # Replaces negative positional values with 0
  refseq$txStart[refseq$txStart < 0] = 0
  refseq$txEnd[refseq$txEnd < 0] = 0
  
  # Appends updated information to output
  bed <- data.frame(refseq$chrom, refseq$txStart, refseq$txEnd,
                    refseq$name2)
  
  # Explicitly returns bed file
  return(bed)
}

# Inner function to generate scores file for one bigwig file
bigwig_to_scores_inner <- function(refseq_file, bw_file, imprinted_file, 
                                   bwtool_folder, output_file,
                                   filter_input = TRUE,
                                   promoter_length = 2500) {
  
  # Converts refseq table to bed file with short transcripts culled
  bed <- refseq_to_bed(refseq_file, promoter_length)
  bed_file <- file.path(dirname(output_file), "loci.bed")
  write.table(bed, file = bed_file, sep = "\t", quote = FALSE,
              row.names = FALSE, col.names = FALSE)
  
  # Executes bwtool on processed refseq bed file and input bw file
  bw_to_counts(bed_file, bw_file, bwtool_folder, output_file)
  
  # Filters certain genes from counts file(s) if specified
  if (filter_input) {
    counts <- filter_counts(output_file, bed_file, imprinted_file)
    write.table(counts, file = output_file, sep = "\t", quote = FALSE,
                row.names = FALSE, col.names = TRUE)
  }
}


# Converts refseq to bed file and outputs scores, with optional promoter
# region scores
bigwig_to_scores <- function(refseq_file, bw_file, imprinted_file, 
                             bwtool_folder, body_output_file,
                             promoter_output_file = NA,
                             filter_input = TRUE,
                             promoter_length = 2500) {
  
  # Generates counts for gene body
  bigwig_to_scores_inner(refseq_file, bw_file, imprinted_file,
                         bwtool_folder, body_output_file,
                         filter_input, promoter_length = 0)
  
  # Generates counts for promoter region if specified
  if (promoter_length > 0) {
    if (is.na(promoter_output_file)) {
      stop("no promoter counts file given")
    }
    bigwig_to_scores_inner(refseq_file, bw_file, imprinted_file,
                           bwtool_folder, promoter_output_file,
                           filter_input, promoter_length)
  }
}

##################
# COMMAND LINE INTERFACE
##################


# Loads required libraries
load_libraries()

# Suppressingfire's SO solution to get executing script.
# Found at http://stackoverflow.com/questions/30468412/dplyr-join-warning-joining-factors-with-different-levels
args <- commandArgs()
current_folder <- dirname(sub("--file=", "", args[grep("--file=", args)]))

# Builds option list using optparse
options = list(
  make_option(c("-i", "--bw_file"), type="character", default=NULL, 
              help="bigwig file to generate counts for"),
  make_option(c("-b", "--body_output_file"), type="character", default=NULL, 
              help="gene body counts file to output"),
  make_option(c("-p", "--promoter_output_file"), type="character", default=NULL, 
              help="promoter region counts file to output"),
  make_option(c("-n", "--no_filter"), action="store_false", default=TRUE, 
              help="disable sex, extra chrom and gene filtering [default= %default]"),
  make_option(c("-l", "--promoter_length"), type="integer", default=5000, 
              help="upstream promoter region length [default= %default]")
)

# Gets options and checks arguments
opt <- parse_args(OptionParser(option_list = options))
if (!file.exists(opt$bw_file)) { stop("bigwig file does not exist") }

# Gets paths to reference files and bwtool
reference_folder <- file.path(dirname(current_folder), "reference")
bin_folder <- file.path(dirname(current_folder), "bin")
refseq_file <- file.path(reference_folder, "mm9_refseq.txt")
imprinted_file <- file.path(reference_folder, "imprinted_genes.tsv")
bwtool_folder <- file.path(bin_folder, "bwtool")

# Extracts variables from args and passes them to bigwig_to_scores
bw_file <- opt$bw_file
body_output_file <- opt$body_output_file
promoter_output_file <- opt$promoter_output_file
filter_input <- opt$no_filter
promoter_length <- opt$promoter_length
bigwig_to_scores(refseq_file, bw_file, imprinted_file, bwtool_folder,
                 body_output_file, promoter_output_file,
                 filter_input, promoter_length)