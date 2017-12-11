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

# Loads all models in a given folder into a dict with model names as keys
get_models <- function(input_folder) {
  
  # Dict of models to build up
  models <- list()
  
  # Loops through all files and reads in all ending with "_model.Rdata"
  model_files <- list.files(input_folder, pattern = "*_model.rds", recursive = FALSE)
  for (file in model_files) {
    model_name <- strsplit(file, "_")[[1]][1]
    model <- readRDS(file.path(input_folder, file))
    models[[model_name]] = model
  }
  
  # Explicitly returns dict of models
  return(models)
}

# Loads all testing sets in a given folder into a single dataframe
get_testing_sets <- function(input_folder) {
  
  # List of testing sets to build up
  sets <- NULL
  
  # Loops through all files and reads in all ending with "_testing.tsv"
  sets_files <- list.files(input_folder, pattern = "*_testing.tsv", recursive = FALSE)
  for (file in sets_files) {
    testing_set <- read.csv(file.path(input_folder, file), sep = "\t")
    
    # Keeps only gene names, actual and predicted allelic bias
    cols_to_keep <- c("name", "status", "prediction")
    testing_set <- testing_set[, names(testing_set) %in% cols_to_keep]
    
    # Renames cols with model name prepended
    model_name <- strsplit(file, "_")[[1]][1]
    names_to_change <- names(testing_set)[names(testing_set) != "name"]
    names(testing_set)[names(testing_set) != "name"] <- 
      paste(model_name, names_to_change, sep = "_")
    
    # Joins sets if already instantiated, or else instantiates it
    if (is.null(sets)) { 
      sets <- testing_set 
    } else {
      sets <- suppressWarnings(full_join(sets, testing_set, by = "name"))
    }
  }
  
  # Sorts sets by alphabetical gene name and reorders columns
  sets <- sets[order(sets$name),]
  sets <- sets[, c(3, 1, 2, 4:ncol(sets))]
  
  # Explicitly returns list of testing sets
  return(sets)
}

# Generates model comparison plots and saves to output folder
generate_model_plots <- function(models, output_folder) {
  
  # Gets resamples between all models and saves plot to file
  resamps <- resamples(models)
  xlim_1 <- c(0.45, 0.75)
  xlim_2 <- c(0.9, 1)
  xlim_3 <- c(0, 0.35)
  suppressWarnings(trellis.par.set(caretTheme()))
  resamps_plot <- bwplot(resamps, layout = c(3, 1),
                         scales = list(cex = 2, 
                                       x = list(relation = "free", rot = 45, cex = 1.3)),
                         # xlim = list(xlim_1, xlim_2, xlim_3),
                         par.strip.text = list(cex = 2),
                         par.settings=simpleTheme(lwd =  10))
  trellis.device(device = "png", filename = file.path(output_folder, "resamps.png"),
                 width = 720, height = 720, pointsize = 24)
  print(resamps_plot)
  dev.off()
  
  # Gets differences between models and saves plot to file
  diffs <- diff(resamps)
  resamps_plot <- bwplot(diffs, layout = c(3, 1),
                         scales = list(cex = 2),
                         par.strip.text = list(cex = 2),
                         par.settings=simpleTheme(lwd =  10))
  trellis.device(device = "png", filename = file.path(output_folder, "diffs.png"),
                 width = 720, height = 720, pointsize = 24)
  print(resamps_plot)
  dev.off()
}

# Generates testing sets comparison plots and saves to output folder
generate_sets_plots <- function(sets, output_folder) {
  
  # Writes joined sets to file
  write.table(sets, file = file.path(output_folder, "joined_testing_sets.tsv"),
              sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)
}

# Creates graphs using model training data and saves to given folder
compare_ml <- function(input_folder, output_folder) {
  
  # Gets models and testing sets by loading all Rdata and tsv files from input folder
  models <- get_models(input_folder)
  sets <- get_testing_sets(input_folder)
  
  # Creates output directory if it doesn't exist
  if (!dir.exists(output_folder)) { dir.create(output_folder) }
  
  # Generates comparison plots and saves them to output folder
  generate_model_plots(models, output_folder)
  generate_sets_plots(sets, output_folder)
}



