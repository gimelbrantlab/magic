###################
# UTILITY FUNCTIONS
###################
## Load libraries
get_package <- function(package_name, repos = "") {
  if(!is.element(package_name, installed.packages()[,1])) {
    if (repos == "") {
      install.packages(package_name)
    } else {
      install.packages(package_name, repos)
    }
  }
  if(!require(package_name, character.only = TRUE, quietly = TRUE)) {
    stop(paste("Cannot load package", package_name))
  }
}

# load all necessary libraries

load_scripts <- function() {
  get_package("dplyr")
  get_package("ggplot2")
  get_package("lattice")
  get_package("caret")
  get_package("plotly")
  get_package("grid")
  get_package("ggrepel")
}
load_scripts()

theme_set(theme_bw(base_size=12))
Sys.setenv("plotly_username"="sachitsaksena")
Sys.setenv("plotly_api_key"="riculov40z")

# load data by providing the path to a file you are loading
load_data <- function(training_path){
  if (grepl(".\\.csv", c(training_path))){
    training_set <- read.csv(training_path, header = TRUE)
  }else {training_set <- read.delim(training_path, header = TRUE)
  }
}

################
# MAIN FUNCTIONS
################

# takes data and column names for MAE/BAE status for each clone


# plot genomic distribution of start positions along each chromosome colored by MAE/BAE status
# Nag data
# INPUT "START" and "STATUS" and "CHROM" ARGUMENTS AS STRING
# takes arguments data (dataset), start (start position column name), status (column name for MAE and BAE status), chrom
# (chromosome column name)
genomic_distribution <- function(data, start, status, chrom){
  distribution_graph <- ggplot(data, aes(x = start, fill = status)) + 
    geom_histogram() +
    facet_wrap(~chrom) + 
    theme(legend.position="right") + scale_x_continuous(labels = abbreviate) + 
    scale_fill_manual(values = c("orange", "blue2", "gray1")) + 
    theme(legend.key = element_blank()) +
    ggtitle("Data Genomic Distribution") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) 
  distribution_graph
}

##########
# EXECUTE
##########
# use load data to import dataset
# load_data()


# example run of genomic distribution function
# genomic_distribution(raw_training_data, "Start", "Abl1_isMAE", "Chr", "path")
# genomic_distribution(full_training, "start", "status", "chrom")

# if that doesn't work because of ggplot2's global environment bug...

# Use this below and replace the words "data" with your data frame, start" with whatever the column name for start position is, replace "status" with whatever the column name of 
# MAE vs BAE status, and chrom with whatever the column name for chromosome name is
# These are in the aes() call and the facet_wrap() call

genomic_distribution <- ggplot(data, aes(x = start, fill = status)) + geom_histogram() +
  facet_wrap(~chrom) + theme(legend.position="right") + 
  scale_x_continuous(labels = abbreviate) + 
  scale_fill_manual(values = c("orange", "blue2", "gray1")) + 
  theme(legend.key = element_blank()) +
  ggtitle("Pre-filtering Genomic Distribution") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) 
genomic_distribution

path <- #set path
ggsave(genomic_distribution, plot = genomic_distribution, device = pdf, path = path)



