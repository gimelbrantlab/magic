## General outline

### MaGIC offers three distinct functionalities

#### Process
Process is a shiny wrapper for the command line utility <b>process.R</b>. This program functions to convert bigWig data from processed ChIP-Seq reads and convert them to mapped scores compatible with analysis of MAE status through analyze.R and model generation through generate.R. Process directly passes the bigWig files inputted to this page and outputs .tsv files with ChIP-Seq region scores by gene. 

#### Analyze
Analyze is a shiny wrapper for the command line utility <b>analyze.R</b>. This program functions to provide predictions for a sample using a previously trained model to determine status for genes within that sample. Simply provide a file of ChIP-Seq enrichment scores from process.R and retrieve predictions. 

#### Generate
We encourage the use of MaGIC 2.0 to improve upon published predictive models for inferring MAE to improve predictive reliability and indicate possible mechanistic implications of epigenetic signatures in MAE. To this end, Generate is a shiny wrapper for the command line utility <b>generate.R</b>. This program functions to allow users to input training data based on monoclonal allelic imbalance data and novel epigenetic signature data to create models with a variety of different algorithms to further optimize the effort to characterize cell-cell allele-specific heterogeneity. 


