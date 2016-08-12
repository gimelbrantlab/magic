#!/usr/bin/env Rscript

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
refseq_to_bed <- function(refseq_file, promoter_length, overlap) {
  
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
  
  # Calculates promoter regions if specified
  if (promoter_length > 0) {
    i <- refseq$strand == "+"
    
    # For "+" strand, assume txStart is TSS
    if (overlap) {
      refseq$txEnd[i] <- refseq$txStart[i] + promoter_length
      refseq$txStart[i] <- refseq$txStart[i] - promoter_length
    } else {
      refseq$txEnd[i] <- refseq$txStart[i]
      refseq$txStart[i] <- refseq$txStart[i] - promoter_length
    }
    
    # For "-" stand, assume txEnd is TSS
    if (overlap) {
      refseq$txStart[!i] <- refseq$txEnd[!i] - promoter_length
      refseq$txEnd[!i] <- refseq$txEnd[!i] + promoter_length 
    } else {
      refseq$txStart[!i] <- refseq$txEnd[!i]
      refseq$txEnd[!i] <- refseq$txEnd[!i] + promoter_length
    }
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
                                   filter_input, promoter_length,
                                   overlap) {
  
  # Converts refseq table to bed file with short transcripts culled
  bed <- refseq_to_bed(refseq_file, promoter_length, overlap)
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
                             overlap = TRUE,
                             promoter_length = 2500) {
  
  # Generates counts for gene body
  bigwig_to_scores_inner(refseq_file, bw_file, imprinted_file,
                         bwtool_folder, body_output_file,
                         filter_input, promoter_length = 0,
                         overlap)
  
  # Generates counts for promoter region if specified
  if (promoter_length > 0) {
    if (is.na(promoter_output_file)) {
      stop("no promoter counts file given")
    }
    bigwig_to_scores_inner(refseq_file, bw_file, imprinted_file,
                           bwtool_folder, promoter_output_file,
                           filter_input, promoter_length,
                           overlap)
  }
}