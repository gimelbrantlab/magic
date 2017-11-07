# Copyright (C) 2017 Dana-Farber Cancer Institute Inc.

# Reads in a bigwig file and a bed file and runs them through bwtool

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

# Removes sex-linked and olfactory receptor genes, as well as genes on
# extra or partially-assembled chromosomes
filter_counts <- function(counts, bed, imprinted_file,
                          filter_olf, filter_chroms, filter_imprinted) {
  
  counts <- remove_extra_chroms(counts)
  if (filter_olf) { counts <- remove_olfactory(counts) }
  if (filter_chroms) { counts <- remove_sex_chroms(counts) }
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
                  "chr16", "chr17", "chr18", "chr19", "chr20", "chr21", "chr22", "chrX", "chrY")
  
  # Removes partially assembled and extra chromosomes
  counts <- counts %>% filter(chrom %in% chrom_list)
  return(counts)
}

remove_sex_chroms <- function(counts) {
  
  # List of chromosomes to keep
  chrom_list <- c("chr1", "chr2", "chr3", "chr4", "chr5",
                  "chr6", "chr7", "chr8", "chr9", "chr10",
                  "chr11", "chr12", "chr13", "chr14", "chr15",
                  "chr16", "chr17", "chr18", "chr19", "chr20", "chr21", "chr22")
  
  # Removes sex-linked chromosomes
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
  
  # Checks to see if any of the arguments are empty
  if((bed_file == "") || (bw_file == "") || (output_file == "")) {
    stop("one or more arguments to bwtool summary are missing, check your input files")
  }
  
  # Builds command to run bwtool and executes it
  bwtool_file <- file.path(bwtool_folder, "bwtool")
  args <- paste("summary", bed_file, bw_file, output_file, 
                "-header", "-with-sum")
  try(system2(bwtool_file, args))
}

# Inner function to generate scores file for one bigwig file
bigwig_to_scores_inner <- function(bed_file, bw_file, imprinted_file, 
                                   bwtool_folder, output_file,
                                   filter_olf, filter_chroms,
                                   filter_imprinted) {

  # Executes bwtool on bed file and input bw file
  bw_to_counts(bed_file, bw_file, bwtool_folder, output_file)
  
  # Reads bed and processed counts files and appends names column
  bed <- read.csv(bed_file, sep = "\t", header = FALSE)
  counts <- read.csv(output_file, sep = "\t", header = TRUE)
  counts <- append_bed_names(counts, bed)
  
  # Filters counts file in various ways and writes out to file
  counts <- filter_counts(counts, bed, imprinted_file,
                          filter_olf, filter_chroms, filter_imprinted)
  write.table(counts, file = output_file, sep = "\t", quote = FALSE,
              row.names = FALSE, col.names = TRUE)
}


# Converts refseq to bed file and outputs scores, with optional promoter
# region scores
bigwig_to_scores <- function(bw_file, imprinted_file, bwtool_folder, 
                             body_output_file, body_bed_file, 
                             promoter_output_file = NA, promoter_bed_file = NA, 
                             filter_olf = TRUE, filter_chroms = TRUE, 
                             filter_imprinted = TRUE) {
  
  # Verifies that the given bed files exist
  if (!file.exists(body_bed_file)) {
    stop(paste("body bed file does not exist for", bw_file))
  }
  if (!is.na(promoter_bed_file) && !file.exists(promoter_bed_file)) {
    stop(paste("promoter bed file does not exist for", bw_file))
  }
  
  # Generates counts for gene body
  bigwig_to_scores_inner(body_bed_file, bw_file, imprinted_file,
                         bwtool_folder, body_output_file, filter_olf, 
                         filter_chroms, filter_imprinted)
  
  # Generates counts for promoter region if specified
  if (!is.na(promoter_bed_file) || !is.na(promoter_output_file)) {
    
    # Checks to see if both the promoter bed and counts files are given
    if (is.na(promoter_bed_file)) {
      stop(paste("no promoter bed file given for", bw_file))
    } else if (is.na(promoter_output_file)) {
      stop(paste("no promoter counts file given for", bw_file))
    }
    
    bigwig_to_scores_inner(promoter_bed_file, bw_file, imprinted_file,
                           bwtool_folder, promoter_output_file, filter_olf, 
                           filter_chroms, filter_imprinted)
  }
}
