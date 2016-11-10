# MaGIC 2.0

This software generates predictions or classifiers for gene expression patterns (MAE or BAE) from
a set of chromatin marks. Primarily based on the work of the [Gimelbrant lab](http://research4.dfci.harvard.edu/gimelbrantlab/Main.html). 

## Requirements

Linux machine with updated R (~3.3.1), the Rscript utility, and a variety of different R packages.
When you run the program, predict_mae.R, you will be asked if you want to install packages you're 
missing.

## General Usage

The program works as follows: you pass predict_mae.R an input folder with mark and control files,
and it spits out an output folder. 

*Input folder*: contains mark and control files in the bigWig file format (.bigWig or .bw). Mark
files contain chromatin mark enrichment data across an organism's genome, generally ChIP-Seq,
and control files contain baseline data for the same experiment. For a given mark name, mark 
files must be named as [mark name]\_mark.bigWig or [mark name]\_mark.bw, and that mark's 
corresponding control file must be named [mark name]\_control.bigWig or [mark name]\_control.bw.

*Output folder*: contains two tables for normalized scores and percentile ranks of predicted or 
reference genes. Scores are normalized to either the length of the gene or the baseline enrichment
data, if available, and percentile ranks are of the normalized scores. If enabled, two subfolders
containing classifiers trained on 80% of the data are output, as well as their predictive performance
on the remaining 20% of the data.

## Example command line usage

*Minimum*:
    Rscript predict_mae.R -i input -r "mm9"
    
*Human genome*:
    Rscript predict_mae.R -i input -o output -r "hg19" -t "human"
    
*Mouse genome with many options*:
    Rscript predict_mae.R -i input -o output -r "mm9" -f -p 2500 -d 0.01 -c -l
    
## Arguments

All arguments are also described via "Rscript predict_mae.R --help"

*-i, --input_folder*:
    Path to input folder, described in general usage
    
*-o, --output_folder*:
    Path to output folder, described in general usage [default output]
    
*-r, --refseq_file*: 
    either a name of a default refseq file ("mm9" or "hg19") *or* a path to a refseq file downloaded 
    from the [UCSC table browser](https://genome.ucsc.edu/cgi-bin/hgTables). IMPORTANT: must *not* contain exonStarts and exonEnds columns
    
*-f, --no_filter*: 
    disable sex, extra chrom and olfactory gene filtering
    
*-p, --promoter_length*: 
    length of promoter region, disables promoter region separation if set to 0 [default 5000]
    
*-d, --drop_percent*: 
    bottom baseline enrichment percentile of genes to drop, as a decimal [default 0.01]
    
*-c, --no_clean_intermediate*: 
    disable automatic removal of intermediate output files
    
*-s, --sampling_method*: 
    resampling method to use when training classifiers, one of "none", "down", or "up"
[default "none"]

*-l, --no_overlap*: 
    disables default option of summing enrichment for *promoter_length* base pairs on either side of the TSS for a promoter region
    
*-t, --training_genes_file*: 
    if generating classifiers, specify "mouse", "human" or the name of a training genes file in the reference folder

*-q, --quiet*: 
    disables console output, do not flag if required packages missing [default FALSE]


## Contact us

Questions, comments and concerns can be directed to [Alexander Gimelbrant](alexander_gimelbrant%40dfci.harvard.edu)
and [Sebastien Vigneau](sebastien.vigneau@gmail.com). Inquiries about how the program works can
be directed to [Henry Ward](henry.neil.ward@gmail.com) and [Sachit Saksena](sachitdsaksena@utexas.edu).
Script written by Henry Ward. Data processing tools written by Sachit Saksena.
