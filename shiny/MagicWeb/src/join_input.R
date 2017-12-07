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

# Removes unnecessary columns from df
clean_cols <- function(df, status_present = FALSE) {
  if ("status" %in% colnames(df)) {
    to_keep <- c("percentile", "norm_sum", "status", "name", "id", "chrom")
  }
  else {
    to_keep <- c("percentile", "norm_sum", "name", "id", "chrom")
  }
  df <- df[to_keep]
  return(df)
}

# Attaches ids, subsets to training genes if applicable, and
# removes unnecessary columns
process_df <- function(df) {
  df <- attach_ids(df)
  return(clean_cols(df))
}

# Joins multiple outputs from normalize_scores.R into one file with
# multiple chromatin marks for each interval
join_input_main <- function(body_files, promoter_files, mark_names, 
                            percentile_output_file, norm_output_file, promoter_length) {
  
  # Reads in lists of data frames and processes each one
  body_dfs <- list()
  promoter_dfs <- list()
  for (i in 1:length(mark_names)) {
    body_dfs[[i]] <- read.csv(body_files[[i]], sep = "\t")
    body_dfs[[i]] <- 
      process_df(body_dfs[[i]])
    
    # Also processes promoter files if they exist
    if (promoter_length > 0) {
      promoter_dfs[[i]] <- read.csv(promoter_files[[i]], sep = "\t")
      promoter_dfs[[i]] <- 
        process_df(promoter_dfs[[i]])
    }
  }
  
  # Data frames built up and output to files
  percentile_df <- NA
  norm_df <- NA
  
  # Ensures that both body and promoter region have same genes.
  # Necessary since normalization could remove different genes
  if (promoter_length > 0) {
    ids_to_keep <- intersect(body_dfs[[1]]$id, promoter_dfs[[1]]$id)
    promoter_dfs[[1]] <- promoter_dfs[[1]][promoter_dfs[[1]]$id %in% ids_to_keep, ]
    body_dfs[[1]] <- body_dfs[[1]][body_dfs[[1]]$id %in% ids_to_keep, ]
    percentile_df <- body_dfs[[1]]
    norm_df <- body_dfs[[1]]
  } else {
    percentile_df <- body_dfs[[1]]
    norm_df <- body_dfs[[1]]
  }
  
  # Names of percentile and norm sum columns
  to_remove <- c("percentile", "norm_sum")
  
  # Removes first percentile and norm cols from dataframes
  percentile_df <- percentile_df[, !names(percentile_df) %in% c(to_remove)]
  norm_df <- norm_df[, !names(norm_df) %in% c(to_remove)]
  
  # Joins each df
  for (i in 1:length(mark_names)) {

    # Appends promoter cols if they exist
    if (promoter_length > 0) {
      
      # Gets relevant variables
      ids_to_keep <- intersect(body_dfs[[i]]$id, promoter_dfs[[i]]$id)
      ids_to_keep <- intersect(ids_to_keep, percentile_df$id)
      promoter_dfs[[i]] <- promoter_dfs[[i]][promoter_dfs[[i]]$id %in% ids_to_keep, ]
      body_dfs[[i]] <- body_dfs[[i]][body_dfs[[i]]$id %in% ids_to_keep, ]
      percentile_df <- percentile_df[percentile_df$id %in% ids_to_keep, ]
      norm_df <- norm_df[norm_df$id %in% ids_to_keep, ]
      
      # Appends promoter columns
      percentile_col <- paste(mark_names[[i]], "promoter_percentile", sep = "_")
      percentile_df[percentile_col] <- promoter_dfs[[i]]["percentile"]
      norm_col <- paste(mark_names[[i]], "promoter_norm_sum", sep = "_")
      norm_df[norm_col] <- promoter_dfs[[i]]["norm_sum"]
      
      # Appends gene body columns
      percentile_col <- paste(mark_names[[i]], "body_percentile", sep = "_")
      percentile_df[percentile_col] <- body_dfs[[i]]["percentile"]
      norm_col <- paste(mark_names[[i]], "body_norm_sum", sep = "_")
      norm_df[norm_col] <- body_dfs[[i]]["norm_sum"]
    } else {
      
      # Gets relevant variables
      ids_to_keep <- intersect(body_dfs[[i]]$id, percentile_df$id)
      body_dfs[[i]] <- body_dfs[[i]][body_dfs[[i]]$id %in% ids_to_keep, ]
      percentile_df <- percentile_df[percentile_df$id %in% ids_to_keep, ]
      norm_df <- norm_df[norm_df$id %in% ids_to_keep, ]
      
      # Appends gene body columns
      percentile_col <- paste(mark_names[[i]], "percentile", sep = "_")
      percentile_df[percentile_col] <- body_dfs[[i]]["percentile"]
      norm_col <- paste(mark_names[[i]], "norm_sum", sep = "_")
      norm_df[norm_col] <- body_dfs[[i]]["norm_sum"]
    }
  }
  
  # Removes id column if present
  percentile_df <- percentile_df[, !(names(percentile_df) %in% c("id"))]
  norm_df <- norm_df[, !(names(norm_df) %in% c("id"))]
  
  # Writes joined, cleaned dataframes out to folder
  write.table(percentile_df, sep = "\t", file = percentile_output_file,
              col.names = TRUE, row.names = FALSE, quote = FALSE)
  write.table(norm_df, sep = "\t", file = norm_output_file,
              col.names = TRUE, row.names = FALSE, quote = FALSE)
}
