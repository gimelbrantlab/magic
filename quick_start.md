# Installation

The following dependencies are required to run MaGIC 2.0:
* Unix-based operating system
* [R installation](https://www.r-project.org/)
* Rscript utility ([comes bundled with R](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/Rscript.html))

# Overview

There are two main use cases for MaGIC 2.0. 

The first case involves processing a bigwig file containing data for any number of chromatin marks, and then making allelic bias predictions on that data using pre-existing classifiers:

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

Like the other programs in the MaGIC 2.0 pipeline, generate.R and analyze.R, process.R is run through the command line. Chromatin-mark data in the [bigwig file format](https://genome.ucsc.edu/goldenpath/help/bigWig.html) is the input file for process.R. 

Three sample input files are included in the MaGIC package's "data" folder. To run those through process.R, we will need to construct a file that identifies those files' corresponding marks and input files. In this case, there are two marks, h3k27me3 and h3k36me3, two mark files, "h3k36me3\_sample.bigWig" and "h3k27me3\_sample.bigWig," and one input file shared by both mark files, "input\_sample.bigWig."

To properly input those files to process.R, we need to create an additional tsv file containing all of the above information. That file is formatted as follows, with entries separated by tabs:

| mark name 	| mark file                 | control file                        	|
|-----------	|-----------------------	|-------------------------------------	|
| h3k27me3  	| h3k27me3\_sample.bigWig 	| input\_sample.bigWig 	|
| h3k36me3  	| h3k36me3\_sample.bigWig 	| input\_sample.bigWig 	|

As a brief aside, note that while input files are generally necessary to make sense of chromatin mark data, sometimes you will want to process data that has already been normalized to an input. In that situation, you would modify the input file detailed above by simply not putting anything into the control file column for the respective mark.

In any case, for now, this file is provided in the data folder under the name "sample\_process\_input.txt."

The next and final step in data processing is to call the "Rscript process.R command." Assuming the current working directory is MaGIC's outer directory, we call the following command:

	Rscript src/process.R -i data/mouse\_process\_input.txt -o data -r "mm9"

This will run all of the mark files listed in process\_input.txt through bwtool to take the mean sum of chromatin mark enrichment per gene, using the mouse mm9 genome data as a reference, and normalize these means to the input data. The processed data will be output to the "data" folder, and the output file that you will pass to analyze.R and generate.R will be named "joined\_scores\_percentile.txt." 

# generate.R

 To use generate.R, we need true allelic bias calls from RNA-Seq data in clonal cell lines of the target organism. The first option is to simply input "human" or "mouse" in the -a paramater of generate.R, which will use data provided in the reference folder to train your model. Alternatively, if you have allelic bias determinations produced via RNA-Seq in the format of a table with gene names and status, you can input a path to that file. 

Now that we have the joined scores file (output from process.R) and true allelic bias calls (see above), all we have to do to generate classifiers is call generate.R on that file. However, there are two distinct ways of generating classifiers.


The first way is useful if you don't have any testing data on hand. The following call to generate.R will generate classifiers based on genes that are subset to those with allelic bias calls in mice (the -a "mouse" argument) that use a randomly-selected 80% of the data for classifier training and the remaining 20% for automated classifier testing.

	Rscript src/generate.R -i data/joined\_scores\_percentile.txt -o data/classifiers -a "mouse"

The second way is useful if you do have other testing data. The following call to generate.R will generate classifiers based on genes that are subset to those with allelic bias calls in mice that use all of the data for classifier training.

	Rscript src/generate.R -i data/joined\_scores\_percentile.txt -o data/classifiers -a "mouse" -p 100

# analyze.R

Finally, we call analyze.R, using the joined scores file (output from process.R) and the default classifiers that are packaged with MaGIC 2.0 as inputs. 

The following command makes allelic bias predictions ("BAE" for bi-allelic expression or "MAE" for random mono-allelic expression) on the joined scores file using the default models pre-packaged with MaGIC 2.0 (the models folder is given by -m models), treating BAE as the positive class:

	Rscript src/analyze.R -i data/joined\_scores\_percentile.txt -o data -m models -po "BAE"


