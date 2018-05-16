#!/usr/bin/env Rscript

# Installs all relevant packages (similar to utils.R)

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


# Installs a package into the given directory if not installed there
get_package <- function(package_name, repos = "http://cran.us.r-project.org",
                        version = NULL, dependencies = NA, lib = NULL) {
  if(is.na(lib)) {
    lib = .libPaths()[1]
  }
  if(!is.element(package_name, installed.packages(lib.loc = lib)[,1])) {
    cat(paste("installing", package_name, "\n"))
    install.package.version(package_name, dependencies, repos, lib)
  }
}

# Loads or installs shiny libraries
install_shiny_libraries <- function(lib = NA) {
  get_package("shiny", lib = lib)
  get_package("markdown", lib = lib)
  get_package("shinythemes", lib = lib)
  get_package("bsplus", lib = lib)
  get_package("GGally", lib = lib)
  get_package("PRROC", lib = lib)
  get_package("shinyFiles", lib = lib)
  get_package("shinyBS", lib = lib)
  cat("finished installing shiny libraries\n")
}

# Loads or installs all required packages for analysis
install_analyze_libraries <- function(lib = NA) {
  get_package("ggplot2", lib = lib)
  get_package("scales", lib = lib)
  get_package("randomForest", lib = lib)
  get_package("kernlab", lib = lib)
  get_package("ddalpha", lib = lib)
  get_package("recipes", lib = lib)
  get_package("caret", lib = lib)
  get_package("lattice", lib = lib)
  get_package("pROC", lib = lib)
  get_package("ada", lib = lib)
  get_package("fastAdaboost", lib = lib)
  get_package("mboost", lib = lib)
  get_package("randomForest", lib = lib)
  get_package("RSNNS", lib = lib)
  get_package("nnet", lib = lib)
  get_package("lattice", lib = lib)
  get_package("optparse", lib = lib)
  get_package("evtree", lib = lib)
  get_package("MASS", lib = lib)
  get_package("dplyr", lib = lib)
  get_package("e1071", lib = lib)
  cat("finished installing analyze.R libraries\n")
}

# Loads or installs all required packages for processing
install_process_libraries <- function(lib = NA) {
  get_package("plyr", lib = lib)
  get_package("dplyr", lib = lib)
  get_package("reshape2", lib = lib)
  get_package("kernlab", lib = lib)
  get_package("ggplot2", lib = lib)
  get_package("ddalpha", lib = lib)
  get_package("recipes", lib = lib)
  get_package("caret", dependencies = TRUE, lib = lib)
  get_package("lattice", lib = lib)
  get_package("diptest", lib = lib)
  get_package("foreach", lib = lib)
  get_package("iterators", lib = lib)
  get_package("doMC", repos = "http://R-Forge.R-project.org", lib = lib)
  get_package("e1071", lib = lib)
  get_package("gridExtra", lib = lib)
  cat("finished installing process.R libraries\n")
}

# Loads or installs all required packages for machine learning
install_generate_libraries <- function(lib = NA) {
  get_package("scales", dependencies = TRUE, lib = lib)
  get_package("ggplot2", dependencies = TRUE, lib = lib)
  get_package("ddalpha", lib = lib)
  get_package("recipes", lib = lib)
  get_package("caret", dependencies = TRUE, lib = lib)
  get_package("doMC", repos = "http://R-Forge.R-project.org", lib = lib)
  get_package("pROC", lib = lib)
  get_package("ada", lib = lib)
  get_package("fastAdaboost", lib = lib)
  get_package("mboost", lib = lib)
  get_package("randomForest", lib = lib)
  get_package("RSNNS", lib = lib)
  get_package("nnet", lib = lib)
  get_package("kernlab", lib = lib)
  get_package("lattice", lib = lib)
  get_package("optparse", lib = lib)
  get_package("dplyr", lib = lib)
  get_package("evtree", lib = lib)
  get_package("MASS", lib = lib)
  get_package("e1071", lib = lib)
  cat("finished installing generate.R libraries\n")
}

# Installs bwtool from source
install_bwtool <- function(bin) {
  prev_wd <- getwd()
  setwd(bin)
  system2("git", args = c("clone", "https://github.com/CRG-Barcelona/libbeato.git"))
  system2("git", args = c("clone", "https://github.com/CRG-Barcelona/bwtool.git"))
  setwd("libbeato/")
  system2("./configure", args = c("--prefix=$HOME", "CFLAGS=\"-g -O0 -I${HOME}/include\"",
                                  "LDFLAGS=-L${HOME}/lib"))
  system2("make")
  system2("make", args = c("install"))
  setwd("../bwtool/")
  system2("./configure", args = c("--prefix=$HOME", "CFLAGS=\"-g -O0 -I${HOME}/include\"",
                                  "LDFLAGS=-L${HOME}/lib"))
  system2("make")
  system2("make", args = c("install"))
  setwd(prev_wd)
}

# Check to see if all packages are installed
check.packages <- function(pkg){
  if(!is.na(lib)) {
    new.pkg <- pkg[!(pkg %in% installed.packages(lib.loc = lib)[, "Package"])]
  } 
  else {
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  }
  if (length(new.pkg))  {
    print("The following packages were not installed, please try to install them manually:")
    print(new.pkg)
  }
  else {
    print("All libraries were installed properly!")
  }
}

# Install packages
install.package.version <- function(package, dependencies = NA, repos = "http://cran.us.r-project.org", lib = NULL) {
  
  version <- get_version(package)
  
  contriburl <- contrib.url(repos)
  available <- available.packages(contriburl)
  
  if (sum(row.names(available) == package) == 1) {
    current.version <- available[package, 'Version']
    if (is.null(version) || version == current.version) {
      install.packages(package, contriburl = contriburl, dependencies = dependencies, repos = repos, lib = lib)
      return()
    }
  }
  
  package.path <- paste(package, "/", package, "_", version, ".tar.gz", sep="")
  package.url <- sprintf("%s/src/contrib/Archive/%s", repos, package.path)
  local.path <- file.path(tempdir(), basename(package.path))
  if (download.file(package.url, local.path) != 0) {
    stop("couldn't download file: ", package.url)
  }
  
  install.packages(local.path, dependencies = dependencies, repos = repos, lib = lib)
}

# Get correct package versions
get_version <- function(package) {
  version <- NA
  if (package=="ada") { version <- "2.0-5" }
  if (package=="bsplus") { version <- "0.1.1" }
  if (package=="caret") { version <- "6.0-79" }
  if (package=="ddalpha") { version <- "1.3.3" }
  if (package=="diptest") { version <- "0.75-7" }
  if (package=="doMC") { version <- "1.3.5" }
  if (package=="dplyr") { version <- "0.7.4" }
  if (package=="e1071") { version <- "1.6-8" }
  if (package=="evtree") { version <- "1.0-6" }
  if (package=="fastAdaboost") { version <- "1.0.0" }
  if (package=="foreach") { version <- "1.4.4" }
  if (package=="GGally") { version <- "1.3.2" }
  if (package=="ggplot2") { version <- "2.2.1" }
  if (package=="gridExtra") { version <- "2.3" }
  if (package=="iterators") { version <- "1.0.9" }
  if (package=="lattice") { version <- "0.20-35" }
  if (package=="markdown") { version <- "0.8" }
  if (package=="MASS") { version <- "7.3-50" }
  if (package=="mboost") { version <- "2.8-1" }
  if (package=="nnet") { version <- "7.3-12" }
  if (package=="optparse") { version <- "1.4.4" }
  if (package=="partykit") { version <- "1.1-1" }
  if (package=="plyr") { version <- "1.8.4" }
  if (package=="pROC") { version <- "1.12.1" }
  if (package=="PRROC") { version <- "1.3" }
  if (package=="randomForest") { version <- "4.6-14" }
  if (package=="recipes") { version <- "0.1.2" }
  if (package=="reshape2") { version <- "1.4.3" }
  if (package=="RSNNS") { version <- "0.4-10" }
  if (package=="scales") { version <- "0.5.0" }
  if (package=="shiny") { version <- "1.0.5" }
  if (package=="shinyBS") { version <- "0.61" }
  if (package=="shinyFiles") { version <- "0.6.2" }
  if (package=="shinythemes") { version <- "1.1.1" }
  if (package=="kernlab") { version <- "0.9-26" }
  return(version)
}

# Installs all required packages to the specified folder and adds folder to known library trees
magic_install <- function(lib, bin) {
  
  # Installs libraries
  get_package("optparse", lib = lib)
  install_process_libraries(lib)
  install_generate_libraries(lib)
  install_analyze_libraries(lib)
  install_shiny_libraries(lib)
  
  # Adds folder to local Rprofile
  if(!is.na(lib)) {
    f <- file(file.path(getwd(), "install_data.txt"))
    writeLines(paste0(getwd(),"/",lib), f)
    close(f)
  }
  
  # Installs bwtool
  install_bwtool(bin)
  cat("finished installing bwtool\n")
  
  # Copies bwtool folder to Shiny subdirectory
  shiny_bin <- file.path(bin, "..", "shiny", "MagicWeb", "bin")
  if (!dir.exists(shiny_bin)) {
    dir.create(shiny_bin)
  }
  file.copy(bin, file.path(shiny_bin, ".."), recursive = TRUE)
  
  # Check if everything is installed
  packages <- c("shiny","markdown","shinythemes","bsplus","GGally","PRROC","shinyFiles","shinyBS","ggplot2","scales","randomForest","kernlab","ddalpha","recipes","caret","lattice","pROC","ada","fastAdaboost","mboost","RSNNS","nnet","optparse","evtree","MASS","dplyr","e1071","plyr","reshape2","diptest","doMC","gridExtra", "foreach", "iterators")
  check.packages(packages)
}


######
# COMMAND LINE INTERFACE
######

# Suppressingfire's SO solution to get executing script.
# Found at https://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script/36777602
args <- commandArgs(trailingOnly=FALSE)
current_folder <- dirname(sub("--file=", "", args[grep("--file=", args)]))
num_args <- 1
if (!is.na(match("--args", args))) {
  args <- args[match("--args", args):length(args)]
  num_args <- length(args)
}

# Gets path to bin folder and creates it if it doesn't exist
bin_folder <- file.path(current_folder, "..", "bin")
if (!dir.exists(bin_folder)) {
  dir.create(bin_folder)
}

# Checks arguments
lib <- NA
if (num_args == 2) {
  lib <- args[2]
  if(!dir.exists(lib)) {
    cat("creating given package installation directory...\n")
    dir.create(lib)
  }
} else if (num_args > 2) {
  stop("too many args, only arg is an optional path to a package install directory")
}

# Installs all required packages
if (as.numeric(strsplit(as.character(numeric_version(getRversion())), ".", fixed = TRUE)[[1]][1]) >= 3) {
  if (as.numeric(strsplit(as.character(numeric_version(getRversion())), ".", fixed = TRUE)[[1]][2]) >= 4) {
    magic_install(lib, bin_folder)
  }
  else {
    print("Your R version is too low (3.4.1 or higher is required), please update R and come back")
  }
} else {
  print("Your R version is too low (3.4.1 or higher is required), please update R and come back")
}
