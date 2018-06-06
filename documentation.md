# Overview

There are two main use cases for MaGIC 2.0. 

The first case involves processing a bigwig file containing data for any number of chromatin marks (to use our models, please make sure you have h3k27me3 and k3k36me3 marks), and then making allelic bias predictions on that data using pre-existing classifiers:

1) Pass bigwig file to process.R
2) Pass output from step 1 to analyze.R

The second case involves processing a bigwig file containing data for any number of chromatin marks, and then generating new allelic bias classifiers using that data:

1) Pass bigwig file to process.R
2) Pass output from step 1 to generate.R

However, for the purposes of this tutorial, we will detail an unrealistic use case that allows us to explore each program in sequence:

1) Pass bigwig file to process.R
2) Pass output from step 1 to generate.R
3) Pass output from step 1 to analyze.R

# process.R

This script processes ChIP-Seq data from [.bigWig/bw file format](https://genome.ucsc.edu/goldenpath/help/bigWig.html) to informative tables primed for analyze.R or generate.R.

*Input* is bigWig files with chromatin marks data (and control file if available) and text input file containing pathnames of mark and control files. The format for the *input file* is as follows, with values separated by tabs and comments indicated by "#":

| mark name 	| mark file                      	| control file                        	|
|-----------	|--------------------------------	|-------------------------------------	|
| h3k27me3  	| h3k27me3_mark.bigWig 	| control.bigWig 	|
| h3k36me3  	| h3k36me3_mark.bigWig 	| control.bigWig 	

As a brief aside, note that while input files are generally necessary to make sense of chromatin mark data, sometimes you will want to process data that has already been normalized to an input. In that situation, you would modify the input file detailed above by simply not putting anything into the control file column for the respective mark.

*Output folder*: contains two tables for normalized scores and percentile ranks of predicted or reference genes. Scores are normalized to either the length of the gene or the baseline enrichment data, if available, and percentile ranks are of the normalized scores. 

### Example command line usage
    
*Minimum*:
```Rscript src/process.R -i data/input.txt -o output -r hg19 -p 0```
    
*With many options*:
```Rscript src/process.R -i data/input.txt -o output -r hg19 -p 2500 -d 0.05 -r hg19 -e -s 3```
    
### Arguments

All arguments are also described via "Rscript src/process.R --help"

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

# generate.R

 To use generate.R, we need true allelic bias calls from RNA-Seq data in clonal cell lines of the target organism. The first option is to simply input "human" or "mouse" in the -a paramater of generate.R, which will use data provided in the reference folder to train your model. Alternatively, if you have allelic bias determinations produced via RNA-Seq in the format of a table with gene names and status, you can input a path to that file. 

Now that we have the joined scores file (output from process.R) and true allelic bias calls (see above), all we have to do to generate classifiers is call generate.R on that file. However, there are two distinct ways of generating classifiers.


The first way is useful if you don't have any testing data on hand. The following call to generate.R will generate classifiers based on genes that are subset to those with allelic bias calls in mice (the -a "mouse" argument) that use a randomly-selected 80% of the data for classifier training and the remaining 20% for automated classifier testing.

	Rscript src/generate.R -i data/joined_scores_percentile.txt -o data/classifiers -a "mouse"

The second way is useful if you do have other testing data. The following call to generate.R will generate classifiers based on genes that are subset to those with allelic bias calls in mice that use all of the data for classifier training.

	Rscript src/generate.R -i data/joined_scores_percentile.txt -o data/classifiers -a "mouse" -p 100
	
*Input file*: file output by process.R

*Output folder*: contains classifiers trained on 80% of the data as well comparisons based on as their predictive performance on the remaining 20% of the data.

### Example command line usage

*Minimum*:
```Rscript src/generate.R -i data/joined_scores_percentile_GM12878.txt```
    
*With many options*:
```Rscript src/generate.R -i data/joined_scores_percentile_GM12878.txt -o output -ta status -sa down -se oneSE -v reference/testing_human_2015.tsv -r best -l ada,svmPoly,rf -p 80 -c 5 -a human```


    
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

# analyze.R

Finally, we call analyze.R, using the joined scores file (output from process.R) and the default classifiers that are packaged with MaGIC 2.0 as inputs. 

The following command makes allelic bias predictions ("BAE" for bi-allelic expression or "MAE" for random mono-allelic expression) on the joined scores file using the default models pre-packaged with MaGIC 2.0 (the models folder is given by -m models), treating BAE as the positive class:

	Rscript src/analyze.R -i data/joined_scores_percentile.txt -o data -m models -po "BAE"

*Input file*: percentiles file output by process.R

*Output folder*: contains predictions on the input file using specified classifiers in a single dataframe

### Example command line usage

*Minimum*:
```Rscript analyze.R -i data/joined_scores_percentile_GM12878.txt -m models```
    
*With many options*:
```Rscript analyze.R -i data/joined_scores_percentile_GM12878.txt -o output -m models -p MAE -r -f reference/hg19_expr_GM12878.txt -l 2500 -s human```
    
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
        
*-r, --remove*:
    remove low expressed genes
    
*-f, --filter*:
    path to file with gene expression values
    
*-l, --length*:
    gene length threshold [default "2500"]
        
*-s, --species*:
    specify human or mouse [default "human"]
    
*-q, --quiet*: 
    disables console output, do not flag if required packages missing [default FALSE]
    
    
### Filtering

Ouput file with BAE/MAE classification predictions can be filtered by gene expression and/or length. To do this, user specifies path to file containing information about gene expression (in any units: RPKM, FPKM, CPM, counts, etc.) and/or length threshold. The format for the file is as follows, with values separated by tabs:

| gene 	| expression                      	|
|-----------	|--------------------------------	|
| A1BG  	| 10 	|
| A1CF  	| 40 	|

All genes are ordered according to their expression (highly expressed genes to low expressed genes), and then botton half of the genes is filtered out. 
The default length threshold is 2500, can be changed using -l option. User can also use -s option to switch between human and mouse lengths filtering.


