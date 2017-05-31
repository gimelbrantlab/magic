# Normalizes the scores output from bigwig_to_scores based on
# either a control bigwig file or the length of each interval.
normalize_scores <- function(scores_file, output_file, dropped_file,
                             mark, gene_region, input_file = NA, 
                             drop_percent = 0.05, drop_abs = 1.0) {
  
  # Opens scores file and converts NA values to 0
  scores <- read.csv(scores_file, sep = "\t", header = TRUE)
  scores[is.na(scores)] <- 0
  
  # Divides scores by control after dropping lowest percentile
  if (!is.na(input_file)) {
    
    # Reads in control file, converts NA values to 0 and generates percentiles
    control <- read.csv(input_file, sep = "\t", header = TRUE)
    control[is.na(control)] <- 0
    control <- control %>% mutate(percentile = rank(sum) / length(sum))
    
    # Removes indices in the given bottom percentile from both data frames and below a specified absolute mean value
    genes_to_keep <- control$name[(control$percentile > drop_percent) & (control$mean >= drop_abs)]
    genes_to_remove <- control$name[(control$percentile <= drop_percent) | (control$mean < drop_abs)]
    control <- control[!control$name %in% genes_to_remove,]
    scores <- scores[!scores$name %in% genes_to_remove,]
    
    # Writes list of dropped genes to file
    for (gene in genes_to_remove) {
      cat_f(paste(gene, "\t", mark, "_", gene_region, sep = ""), dropped_file)
    }
    
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