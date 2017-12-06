# MaGIC 2.0

This software generates predictions or classifiers for gene expression patterns (MAE or BAE) from
a set of chromatin marks. Primarily based on the work of the [Gimelbrant lab](https://gimelbrantlab.dfci.harvard.edu/). 

## Requirements

Linux machine with updated R (~3.3.1), the Rscript utility, and a variety of different R packages.
When you run the program, predict_mae.R, you will be asked if you want to install packages you're 
missing.

## Process

This script processes ChIP-Seq/MBD-seq/etc. data from .bigWig/bw file format to informative tables primed for analyze.R or generate.R.

*Input file*: contains pathnames of mark and control files in the bigWig file format (.bigWig or .bw). Mark
files contain chromatin mark enrichment data across an organism's genome, generally ChIP-Seq,
and control (otherwise known as input) files contain baseline data for the same experiment. The format for the input file is as follows, with values separated by tabs and comments indicated by "#":

| mark name 	| mark file                      	| control file                        	|
|-----------	|--------------------------------	|-------------------------------------	|
| h3k27me3  	| input_dir/h3k27me3_mark.bigWig 	| mouse_input/h3k27me3_control.bigWig 	|
| h3k36me3  	| input_dir/h3k36me3_mark.bigWig 	| mouse_input/h3k27me3_control.bigWig 	


*Output folder*: contains two tables for normalized scores and percentile ranks of predicted or reference genes. Scores are normalized to either the length of the gene or the baseline enrichment data, if available, and percentile ranks are of the normalized scores. 

### Example command line usage

*Minimum*:
    Rscript process.R -i input\_file -r "mm9"
    
*Human genome*:
    Rscript process.R -i input\_file -o output -r "hg19"
    
*Mouse genome with many options*:
    Rscript process.R -i input\_file -o output -r "mm9" -f -p 2500 -d 0.01 -e -l -m -s 3
    
### Arguments

All arguments are also described via "Rscript process.R --help"

*-i, --input_folder*:
    Path to input folder, described earlier
    
*-o, --output_folder*:
    Path to output folder, described earlier [default output]
    
*-r, --refseq_file*: 
    either a name of a default refseq file ("mm9" or "hg19") *or* a path to a refseq file downloaded from the [UCSC table browser](https://genome.ucsc.edu/cgi-bin/hgTables). IMPORTANT: must *not* contain exonStarts and exonEnds columns
    
*-b, --bed_file*:
    specifies the path of a custom bed file you want to use in place of a refseq file - if given, the refseq file argument is ignored
    
*-p, --promoter_length*: 
    length of promoter region, disables promoter region separation if set to 0 [default 5000]
    
*-d, --drop_percent*: 
    bottom baseline enrichment percentile of genes to drop, as a decimal [default 0.01]

*-a, --drop_absolute*:
    bottom baseline enrichment raw mean of genes to drop [default 1.0]
    
*-e, --no_clean_intermediate*: 
    disable automatic removal of intermediate output files

*-l, --no_overlap*: 
    disables default option of summing enrichment for *promoter_length* base pairs on either side of the TSS for a promoter region
    
*-f, --no_filter_olf*: 
    disable olfactory receptor gene filtering
    
*-c, --no_filter_chrom*: 
    disable extra, partially-assembled and sex-chromosome gene filtering
    
*-m, --no_filter_imprinted*: 
    disable imprinted gene filtering
  
*-s, --cores*:   
    number of cores to use for data processing 

*-q, --quiet*: 
    disables console output, do not flag if required packages missing [default FALSE]

## Analyze

*Input file*: percentiles file output by process.R

*Output folder*: contains predictions on the input file using specified classifiers in a single dataframe

### Example command line usage

*Minimum*:
    Rscript analyze.R -i "input_file.tsv" -m models 
    
*With many options*:
    Rscript analyze.R -i "input_file.tsv" -o output -m models -po "BAE"
    
### Arguments

All arguments are also described via "Rscript analyze.R --help"

*-i, --input_file*:
    path to input file, described in general usage
    
*-o, --output_folder*:
    path to output folder, described in general usage [default output]

*-m, --models_folder*:
    contains models output from generate.R

*-ex, --excluded_models*: 
    list of models to exclude from folder, separated by commas

*-p, --positive_class*:
    name of target feature's positive class [default "MAE"]
    
*-f, --filter*:
    path to file with gene expression values and/or length
    
*-l, --length*:
    gene length threshold [default "2500"]
    
*-q, --quiet*: 
    disables console output, do not flag if required packages missing [default FALSE]
    
    
### Filtering

Ouput file with BAE/MAE classification predictions can be filtered by gene expression and/or length. To do this, user specifies path to file containing information about gene expression (in any units: RPKM, FPKM, CPM, counts, etc.) and/or length. If file contains only expression values, the filtering will be done only by expression. If file contains only gene lengths, the filtering will be done only by length. The format for the file is as follows, with values separated by tabs:

| gene 	| expression                      	| length                        	|
|-----------	|--------------------------------	|-------------------------------------	|
| A1BG  	| 10 	| 2400 	|
| A1CF  	| 40 	| 2600 	

All genes are ordered according to their expression (highly expressed genes to low expressed genes), and then botton half of the genes is filtered out. 
The default length threshold is 2500, can be changed using -l option. 

## Generate

This script allows for user-generated parameters for training a model. 
This can include ChIP-Seq/MAP-Seq/etc. data files that have been processed with process.R. 
These training genes  with MAE/BAE allelic bias calls determined by RNA-Seq are assigned scores from the 
user-provided data and generates a new set of models trained on this data using different classifers, with a corresponding comparison.

*Input file*: file output by process.R

*Output folder*: contains classifiers trained on 80% of the data as well comparisons based on as their predictive performance on the remaining 20% of the data.

### Example command line usage

*Minimum*:
    Rscript generate.R -i "input_file.tsv"
    
*With many options*:
    Rscript generate.R -i "input_file.tsv" -o output -ta "status" -sa "down" -se "oneSE" -v testing_human_2015.tsv
    
### Arguments

All arguments are also described via "Rscript generate.R --help"

*-i, --input_file*:
    path to input file, described in general usage
    
*-o, --output_folder*:
    path to output folder, described in general usage [default output]

*-t, --target_feature*:
    name of column in dataset with feature to classify by [default "status"]

*-m, --metric*: 
    metric to train on, either "Kappa", "Accuracy", or for data with only two classes "ROC"
[default "Kappa"]

*-a, --training_genes_file*: 
    subsets data to preset file with training genes, to use specify "mouse", "human" or the name of a training genes file in the reference folder [default "none"]

*-s, --sampling_method*: 
    resampling method to use when training classifiers, one of "none", "down", or "up" [default "none"]

*-r, --selection_rule*:
    caret rule used to select the best model [default "best"]
    
*-p, --training_percent*: 
    percent of data to use as training set, between 0 and 100 - uses all of data as training set if 100 given [default 80]

*-c, --cross_validation*: 
    number of times to run cross-validation [default 5]

*-l, --model_list*:
    list of model algorithms to test [default "glmStepAIC, rf, nnet, rpart, svmPoly,
                  evtree, knn, ada, mlpML"]

*-v, --validation_file*:
    path to tsv file with genes and matching BAE/MAE status and chromatin data       
    
*-q, --quiet*: 
    disables console output, do not flag if required packages missing [default FALSE]
    
 ### Validation
 
 To test model performance on external control file, users can use option -v providing the path to control file. Control file should have columns with gene names, status ("BAE"/"MAE") and chromatin marks percentiles, same as used for model training (should be produced by process.R as then merged with true genes status). 
 
 | gene 	| chr | status | h3k27me3_percentile | h3k36me3_percentile | 
|-----------	|--------------------------------	|-------------------------------------	|-------------------------------------	|-------------------------------------	|
| AATF  	| chr17 	| BAE 	| 0.332400069 	| 0.885768436
| ABAT  	| chr16 	| MAE 	| 0.578437285 	| 0.438146106
| ABCA7  	| chr19  	| BAE  	| 0.143047898  	| 0.615609924

With -v option on, model_name_to_validation.txt file is created for each model containg information about model performance. Additionally, file summary_models.tsv is created with table of perfomance metrics of all models.

If no validation file is provided, validation is performed on the testing portion of the training data (the fraction is controlled by -p parameter). Is p is set to 100 (meaning that 100% of training data is used for training), no validation is performed (and files model_name_to_validation.txt and summary_models.tsv are not created).

