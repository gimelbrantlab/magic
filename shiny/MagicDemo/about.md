# MaGIC 2.0

This software generates predictions or classifiers for gene expression patterns (MAE or BAE) from a set of chromatin marks. Primarily based on the work of the [Gimelbrant lab](https://gimelbrantlab.dfci.harvard.edu/). 

## Requirements

Linux or Mac machine with updated R (~3.3.1), the Rscript utility, and a variety of different R packages. When you run any of the scripts, you will be asked if you want to install packages you're missing.

## General Usage

**Process.R** concatenates and normalizes bigwig files into a clean table used in analyze.R and generate.R

**Generate.R** creates classifiers based on processed data and compares them

**Analyze.R** runs classifiers on processed data to make predictions

## Contact us

Questions, comments and concerns can be directed to [Alexander Gimelbrant](alexander_gimelbrant%40dfci.harvard.edu)
and [Sebastien Vigneau](sebastien.vigneau@gmail.com). Inquiries about how the program works can
be directed to [Henry Ward](henry.neil.ward@gmail.com) and [Sachit Saksena](sachitdsaksena@utexas.edu).
Script written by Henry Ward. Data processing tools written by Sachit Saksena.
