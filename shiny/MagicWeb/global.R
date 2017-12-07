

rm(list=ls())
######
# VARIABLES
######

src_folder <- file.path("src")
models_folder <- file.path("models")
reference_folder <- file.path("reference")

######
# LIBRARIES AND SCRIPTS
######

source('directoryInput.R')
source("utils.R")
source("fig2a_script.R")

# load shiny libraries from utils.R
load_shiny_libraries()
