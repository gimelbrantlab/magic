

# Removes sex-linked and olfactory receptor genes, as well as genes on
# extra or partially-assembled chromosomes
filter_counts <- function(counts, bed, imprinted_file,
                          filter_olf, filter_chroms, filter_imprinted) {
  
  if (filter_olf) { counts <- remove_olfactory(counts) }
  if (filter_chroms) { counts <- remove_extra_chroms(counts) }
  if (filter_imprinted) { counts <- remove_imprinted(counts, imprinted_file) }
  
  # Explicitly returns filtered counts
  return(counts)
}

# Removes imprinted genes from a counts df
remove_imprinted <- function(counts, imprinted_file) {
  imprinted <- read.csv(imprinted_file, sep = "\t")
  counts <- counts %>% filter(!(name %in% imprinted[,1]))
  return(counts)
}

# Removes olfactory receptors genes from a counts df
remove_olfactory <- function(counts) {
  counts <- counts %>% filter(!(grepl('^Olfr', name)))
  return(counts)
}

# Removes sex-linked, partially assmebled and extra chromosomes
# from a counts df
remove_extra_chroms <- function(counts) {
  
  # List of chromosomes to keep
  chrom_list <- c("chr1", "chr2", "chr3", "chr4", "chr5",
                  "chr6", "chr7", "chr8", "chr9", "chr10",
                  "chr11", "chr12", "chr13", "chr14", "chr15",
                  "chr16", "chr17", "chr18", "chr19")
  
  # Removes sex-linked, partially assembled and extra chromosomes
  counts <- counts %>% filter(chrom %in% chrom_list)
  return(counts)
}

# Appends name column from bed df to counts df
append_bed_names <- function(counts, bed) {
  
  # Renames column names in counts and bed dfs
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
                                   promoter_length, overlap,
                                   filter_olf, filter_chroms,
                                   filter_imprinted) {
  
  bed <- ""
  
  # Gets file extension of refseq or bed file
  components <- strsplit(refseq_file, "\\.")[[1]]
  extension <- tolower(components[length(components)])
  
  # If custom bed file given, uses that instead of the refseq file
  if (extension == "bed") {
    bed <- read.csv(refseq_file, sep = "\t")
    
  # Else, converts refseq table to bed file with short transcripts culled
  } else {
    bed <- refseq_to_bed(refseq_file, promoter_length, overlap)
  }
  
  bed_file <- file.path(dirname(output_file), "loci.bed")
  write.table(bed, file = bed_file, sep = "\t", quote = FALSE,
              row.names = FALSE, col.names = FALSE)

  # Executes bwtool on bed file and input bw file
  bw_to_counts(bed_file, bw_file, bwtool_folder, output_file)
  
  # Opens processed counts file and appends names column
  counts <- read.csv(output_file, sep = "\t")
  counts <- append_bed_names(counts, bed)
  
  # Filters counts file in various ways and writes out to file
  counts <- filter_counts(counts, bed, imprinted_file,
                          filter_olf, filter_chroms, filter_imprinted)
  write.table(counts, file = output_file, sep = "\t", quote = FALSE,
              row.names = FALSE, col.names = TRUE)
}


# Converts refseq to bed file and outputs scores, with optional promoter
# region scores
bigwig_to_scores <- function(refseq_file, bw_file, imprinted_file, 
                             bwtool_folder, body_output_file,
                             promoter_output_file = NA, promoter_length = 2500,
                             overlap = TRUE, filter_olf = TRUE, 
                             filter_chroms = TRUE, filter_imprinted = TRUE) {
    
  # Generates counts for gene body
  bigwig_to_scores_inner(refseq_file, bw_file, imprinted_file,
                         bwtool_folder, body_output_file, 0, 
                         overlap, filter_olf, filter_chroms,
                         filter_imprinted)
  
  # Generates counts for promoter region if specified
  if (promoter_length > 0) {
    if (is.na(promoter_output_file)) {
      stop(paste("no promoter counts file given for", bw_file))
    }
    bigwig_to_scores_inner(refseq_file, bw_file, imprinted_file,
                           bwtool_folder, promoter_output_file, promoter_length, 
                           overlap, filter_olf, filter_chroms,
                           filter_imprinted)
  }
}