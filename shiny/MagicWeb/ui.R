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


######
### UI LIBRARIES
###### 

# Gets custom install directory if used in install.R
lib <- get_install_dir(paste0(getwd(), "/../../"))

if (is.null(lib)) lib <- NA

if (is.na(lib)) {
  library(shiny)
  library(markdown)
  library(shinythemes)
  library(GGally)
  library(shinyFiles)
  library(bsplus)
}else {
  library(shiny, lib.loc = lib)
  library(markdown, lib.loc = lib)
  library(shinythemes, lib.loc = lib)
  library(GGally, lib.loc = lib)
  library(shinyFiles, lib.loc = lib)
  library(bsplus, lib.loc = lib)
}


######
### UI GLOBALS
######

# All ui-specific global variables
organism <- c("human", "mouse", "other")
assembly <- c("mm9","mm10", "other")
assembly <- c(assembly, "hg19", "hg38", "other")
tg_names <- get_names(reference_folder, pattern = "*_tg.tsv")
tg_names <- c("human", "mouse", "none", "other")
model_names <- get_names(models_folder, pattern = "*_model.rds")
acceptable_file_types <- c("text/plain",
                           "text/csv",
                           "text/comma-separated-values",
                           ".csv",
                           "text/tsv",
                           "text/tab-separated-values",
                           ".tsv")
selection_rules <- c("best", "oneSE", "tolerance")
metric_names <- c("Kappa", "Accuracy", "ROC")
sampling_method_names <- c("none", "down", "up")
positive_classes <- c("MAE", "BAE", "other")
model_list <- c("ada", "svmPoly", "rf", "nnet", "rpart", "mlpML", "knn", "evtree", "glmStepAIC")
filtering <- c("olfactory receptor genes", "sex chromosomes", "imprinted genes")

if (!is.na(lib)) {
  load_process_libraries(lib)
  load_analyze_libraries(lib)
  load_generate_libraries(lib)
  load_shiny_libraries(lib)
} else {
  load_process_libraries()
  load_analyze_libraries()
  load_generate_libraries()
  load_shiny_libraries()
}

######
### UI
######


shinyUI(

  tagList(
    # make navbar look cool
    navbarPage(
      title = "", id="main_panel",
      theme = shinytheme("flatly"),
      # source tabPanels
      source("ui/ui-main-tab.R", local=TRUE)$value,
      source("ui/ui-process-tab.R", local=TRUE)$value,
      source("ui/ui-generate-tab.R", local=TRUE)$value,
      source("ui/ui-analyze-tab.R", local=TRUE)$value,
      source("ui/ui-tutorial-tab.R", local=TRUE)$value
    ),
    # activate tooltips, popovers
    use_bs_tooltip(),
    use_bs_popover()
  )
)
