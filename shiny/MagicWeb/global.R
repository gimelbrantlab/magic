

rm(list=ls())
######
# VARIABLES
######

src_folder <- file.path("..", "..", "src")
models_folder <- file.path("..", "..", "models")
reference_folder <- file.path("..", "..", "reference")

######
# LIBRARIES AND SCRIPTS
######

source('directoryInput.R')
source(file.path(src_folder, "utils.R"))
source("fig2a_script.R")
