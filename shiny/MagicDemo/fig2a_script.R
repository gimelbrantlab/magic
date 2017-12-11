### FIGURE 2a
### Sachit Saksena
### 04/26/16


###########
# HELPER FUNCTIONS
###########
# easy load data func
load_data <- function(training_path){
  if (grepl(".\\.csv", c(training_path))){
    training_set <- read.csv(training_path, header = TRUE)
  }else {training_set <- read.delim(training_path, header = TRUE)
  }
  return(training_set)
}


# Checks if a package is installed, and installs it if specified.
# Also loads the package
get_package <- function(package_name, repos = "", dependencies = FALSE) {
  if(!is.element(package_name, installed.packages()[,1])) {
    if (repos == "") {
      install.packages(package_name, dependencies)
    } else {
      install.packages(package_name, repos)
    }
  }
  if(!suppressMessages(require(package_name, character.only = TRUE))) {
    stop(paste("Cannot load package", package_name))
  }
}

# Loads or installs all required packages
load_libraries <- function() {
  get_package("ggplot2")
  get_package("scales")
  get_package("kernlab")
  get_package("caret")
  get_package("lattice")
  get_package("ada")
  get_package("lattice")
  get_package("optparse")
  get_package("evtree")
  get_package("plyr")
  get_package("dplyr")
  get_package("pROC")
  get_package("PRROC")
}

# exwcute load library script
load_libraries()

# easy load data func
load_data <- function(training_path){
  if (grepl(".\\.csv", c(training_path))){
    training_set <- read.csv(training_path, header = TRUE)
  }else {training_set <- read.delim(training_path, header = TRUE)
  }
  return(training_set)
}


# Checks if a package is installed, and installs it if specified.
# Also loads the package
get_package <- function(package_name, repos = "", dependencies = FALSE) {
  if(!is.element(package_name, installed.packages()[,1])) {
    if (repos == "") {
      install.packages(package_name, dependencies)
    } else {
      install.packages(package_name, repos)
    }
  }
  if(!suppressMessages(require(package_name, character.only = TRUE))) {
    stop(paste("Cannot load package", package_name))
  }
}

# Loads or installs all required packages
load_libraries <- function() {
  get_package("ggplot2")
  get_package("scales")
  get_package("kernlab")
  get_package("caret")
  get_package("lattice")
  get_package("ada")
  get_package("lattice")
  get_package("optparse")
  get_package("evtree")
  get_package("plyr")
  get_package("dplyr")
  get_package("pROC")
}

# exwcute load library script
load_libraries()


############
# DATA PROCESSING TOOLS
############

# function for filtering expression
# use num if median=FALSE
filter_expression <- function(df, expression_column, median = TRUE, num){
  # set cutoff value as the median
  # make sure data types match across factors
  if(median == TRUE){
    cutoff <- median(as.numeric(as.character(df[,2])))
    print(cutoff)
  } else{
    cutoff <- num
  }
  df %>% filter(as.numeric(as.character(df[,2])) > cutoff) -> df
  return(df)
}

# rename annoying column names
rename_columns <- function(df){
  colnames(df)[grep("Gene|*gene*", colnames(df))] <- "gene"
  colnames(df)[grep("Chr", colnames(df))] <- "chr"
  colnames(df)[grep("*predictions*", colnames(df))] <- "status"
  colnames(df)[grep("Start", colnames(df))] <- "start"
  return(df)
}

# take input of cell type and number of clones
select_cell_type <- function(df, cell_type_string, number_of_clones, start=TRUE){
  # isolate cell type
  if (start==TRUE){
    df %>% select(grep("Gene|gene", colnames(df)), 
                  grep("Chr|chr", colnames(df)), 
                  grep(cell_type_string, colnames(df)), 
                  grep("start|Start", colnames(df))) -> df_culled
  } else if (start == FALSE){
    df %>% select(grep("Gene|gene", colnames(df)), 
                  grep(cell_type_string, colnames(df))) -> df_culled
  }
  # rename columns for any amount of clones
  for (i in 1:number_of_clones){
    colnames(df_culled)[grep(paste("is_unbiased_", cell_type_string, i, sep = ""), 
                             colnames(df_culled))] <- paste("clone", i, "BAE", sep = "")
    colnames(df_culled)[grep(paste("is_biased_", cell_type_string, i, sep = ""), 
                             colnames(df_culled))] <- paste("clone", i, "MAE", sep = "")
    colnames(df_culled)[grep(paste(".", i, "_isBAE", sep = ""), 
                             colnames(df_culled))] <- paste("clone", i, "BAE", sep = "")
    colnames(df_culled)[grep(paste(".", i, "_isMAE", sep = ""), 
                             colnames(df_culled))] <- paste("clone", i, "MAE", sep = "")
    colnames(df_culled)[grep(paste(".", i, "_Bias", sep = ""), 
                             colnames(df_culled))] <- paste("clone", i, "Bias", sep = "")
    colnames(df_culled)[grep(paste("K27divIN"), colnames(df_culled))] <- paste("h3k27me3_percentile", sep="")
    colnames(df_culled)[grep(paste("K36divIN"), colnames(df_culled))] <- paste("h3k36me3_percentile", sep="")
    colnames(df_culled)[grep(paste("_passFilter"), colnames(df_culled))] <- paste("filtering", sep="")
  }
  df_culled <- rename_columns(df_culled)
  return(df_culled)
}


# split data 
# takes df (dataframe) processed through "select_cell_type", if stringent=TRUE
# more stringent status assignments for MAE/BAE is used 
# if bias_test=TRUE, this function appends a bias test on to 
assign_status <- function(df, stringent=FALSE, start=TRUE){
  if (start==TRUE){
    #  stringency
    if (stringent==FALSE){
      # create MAE dataset
      df %>% filter(clone1MAE == 1 | clone2MAE == 1) %>% 
        mutate(status = "MAE") %>% 
        select(grep("Gene|gene", colnames(df)), grep("Chr|chr", colnames(df)), status, start) -> MAE
      
      # create BAE dataset
      df %>% filter((clone1BAE == 1 & clone2MAE != 1) | 
                      (clone2BAE == 1 & clone1MAE != 1)) %>% 
        mutate(status = "BAE") %>% select(grep("Gene|gene", colnames(df)), grep("Chr|chr", colnames(df)),
                                          status, start) -> BAE 
      
      # merged data, retaining all genes
      temp <- merge(MAE, BAE, all = TRUE)
      return(temp)
    } else {
      # append bias test column
      df <- mutate(df, bias_test = (.5-clone1Bias) * (.5-clone2Bias))
      # create random MAE dataset
      df %>% filter((clone1MAE == 1 & clone2BAE == 1) | 
                      (clone2MAE == 1 & clone1BAE == 1) | 
                      ((clone1MAE == 1 & clone2MAE == 1) & 
                         bias_test < 0)) %>% mutate(status = "MAE") %>% 
        select(grep("Gene|gene", colnames(df)), grep("Chr|chr", colnames(df)), status, start) -> MAE
      #create BAE dataset
      df %>% filter((clone1BAE == 1 & clone2MAE != 1) | 
                      (clone2BAE == 1 & clone1MAE !=1)) %>% 
        mutate(status = "BAE") %>%   select(grep("Gene|gene", colnames(df)), grep("Chr|chr",
                                                                                  colnames(df)), status, start) -> BAE
      
      #merged data retaining all genes
      temp <- merge(MAE, BAE, all = TRUE)
      return(temp)
    }
    # if start position information is not needed 
  } else if (start==FALSE) {
    if (stringent==FALSE){
      # create MAE dataset
      df %>% filter(clone1MAE == 1 | clone2MAE == 1) %>% mutate(status = "MAE") %>% 
        select(grep("Gene|gene", colnames(df)), grep("Chr|chr", colnames(df)), status) -> MAE
      
      # create BAE dataset
      df %>% filter((clone1BAE == 1 & clone2MAE != 1) | 
                      (clone2BAE == 1 & clone1MAE != 1)) %>% 
        mutate(status = "BAE") %>% select(grep("Gene|gene", colnames(df)), 
                                          grep("Chr|chr", colnames(df)), status) -> BAE 
      
      # merged data, retaining all genes
      temp <- merge(MAE, BAE, all = TRUE)
      return(temp)
      
    } else {
      # append bias test column
      df <- mutate(df, bias_test = (.5-clone1Bias) * (.5-clone2Bias))
      # create random MAE dataset
      df %>% filter((clone1MAE == 1 & clone2BAE == 1) | 
                      (clone2MAE == 1 & clone1BAE == 1) | 
                      ((clone1MAE == 1 & clone2MAE == 1) & 
                         bias_test < 0)) %>% 
        mutate(status = "MAE") %>% select(grep("Gene|gene", colnames(df)), 
                                          grep("Chr|chr", colnames(df)), status) -> MAE
      #create BAE dataset
      df %>% filter((clone1BAE == 1 & clone2MAE != 1) | 
                      (clone2BAE == 1 & clone1MAE !=1)) %>% 
        mutate(status = "BAE") %>%   
        select(grep("Gene|gene", colnames(df)), grep("Chr|chr", colnames(df)), status) -> BAE
      
      #merged data retaining all genes
      temp <- merge(MAE, BAE, all = TRUE)
      return(temp)
    }
  }
}


# plot rough estimate of decision boundary 
decisionPlot <- function(model, 
                         data, 
                         class=NULL, 
                         predict_type="raw", 
                         resolution=100){
  
  # subset data by class
  if(!is.null(class)){
    cl <- data[,class]
  } else{
    cl <- 1
  }
  # 
  data_sub <- data[,2:3]
  data_sub <- na.omit(data_sub)
  
  # make grid
  k <- length(unique(cl))
  
  # plot intial data distribution 
  plot(data_sub, col = data[,1]) #col=as.integer(cl)+1L) #col=as.integer(cl)+1L, pch=as.integer(cl)+1L)
  
  # make a grid to evaluate decisions over (resolution changes stap size and evaluation speed)
  r <- sapply(data_sub, range, na.rm=TRUE)
  xs <- seq(min(data_sub[,1]), max(data_sub[,1]), length.out=resolution)
  ys <- seq(min(data_sub[,2]), max(data_sub[,2]), length.out=resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time=resolution))
  
  # resolve column names
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  # predict using model with class labels
  p <- predict(model, g, type=predict_type)
  
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  z <- matrix(as.integer(p), nrow=resolution, byrow=TRUE)
  contour(xs,ys,z,add=TRUE,drawlabels = FALSE, lwd=2, levels=(1:(k-1))+.5)
  invisible(z)
}


##################
# PRECISION-RECALL FUNCTIONS
##################

# calculate precision recall=
calc_auprc <- function(model,data, status){
  
  index_class2 <- data$status == "MAE"
  index_class1 <- data$status == "BAE"
  
  # run predictions 
  predictions <- predict(model, data, type="prob")
  # create pr.curve object
  # in this example, MAE is considered the positive class, so precision and recall are 
  # indexed with MAE
  pr.curve(predictions$MAE[index_class2],
           predictions$MAE[index_class1],
           curve = TRUE,
           minStepSize = 3)
}




#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################




