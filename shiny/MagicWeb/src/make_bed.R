# Copyright (C) 2017 Dana-Farber Cancer Institute Inc.

# Reads a refseq table in and outputs a properly formatted bed file

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

# Reads either a custom bed file or a refseq table and writes
# a bed file out as "loci.bed"
make_bed <- function(refseq_file, output_file, promoter_length, 
                     overlap) {
  
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
  
  write.table(bed, file = output_file, sep = "\t", quote = FALSE,
              row.names = FALSE, col.names = FALSE)
  
}