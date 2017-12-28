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
                        dependencies = NA, lib = NULL) {
  if(is.na(lib)) {
    lib = .libPaths()[1]
  }
  if(!is.element(package_name, installed.packages(lib.loc = lib)[,1])) {
    cat(paste("installing", package_name, "\n"))
    if (is.null(lib)) {
      if (repos == "") {
        install.packages(pkgs = package_name, dependencies = dependencies)
      } else {
        install.packages(pkgs = package_name, repos = repos)
      }
    } else {
      if (repos == "") {
        install.packages(pkgs = package_name, dependencies = dependencies, lib = lib)
      } else {
        install.packages(pkgs = package_name, repos = repos, lib = lib)
      }
    }
  }
}

# Loads or installs shiny libraries
install_shiny_libraries <- function(lib = NA) {
  get_package("shiny", lib = lib)
  get_package("markdown", lib = lib)
  get_package("shinythemes", lib = lib)
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
  get_package("caret", dependencies = TRUE, lib = lib)
  get_package("lattice", lib = lib)
  get_package("diptest", lib = lib)
  get_package("doMC", repos = "http://R-Forge.R-project.org", lib = lib)
  get_package("e1071", lib = lib)
  get_package("gridExtra", lib = lib)
  cat("finished installing process.R libraries\n")
}

# Loads or installs all required packages for machine learning
install_generate_libraries <- function(lib = NA) {
  get_package("scales", dependencies = TRUE, lib = lib)
  get_package("ggplot2", dependencies = TRUE, lib = lib)
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
    writeLines(lib, f)
    close(f)
  }

  # Installs bwtool
  install_bwtool(bin)
  cat("finished installing bwtool\n")
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
magic_install(lib, bin_folder)


