

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

#source('directoryInput.R')
source("src/utils.R")
source("fig2a_script.R")

# Gets custom install directory if used in install.R
lib <- get_install_dir(paste0(getwd(), "/../../"))

# Loads optparse
load_initial_libraries(lib)

# load shiny libraries from utils.R
load_shiny_libraries(lib)
