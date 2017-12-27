# MaGIC 2.0

This software generates predictions or classifiers for monoallelic gene expression (MAE) from
chromatin mark enrichment data. It is primarily based on the work of the [Gimelbrant lab](https://gimelbrantlab.dfci.harvard.edu/).

## Requirements

Linux/Mac machine with updated R (~3.4.x) and git. Please see the [instructions](https://github.com/gimelbrantlab/magic/blob/master/Installation.md) if you don't have git or R installed. 

## MaGIC installation

Installing MaGIC should just be a matter of a few commands.  [bwtool](https://github.com/CRG-Barcelona/bwtool/wiki) is the only requirement.

To install MaGIC system-wide (i.e. with root/sudo access): 
```
git clone https://github.com/gimelbrantlab/magic.git
cd magic
Rscript src/install.R
```
This command will install MaGIC to run with command line. For visualization purposes, we also developed Shiny App that can be run with the following command: 

```
R -e "shiny::runApp('shiny/MagicWeb/', launch.browser = T)"
```
If you change your mind and want to uninstall MaGIC, you can just delete the whole folder:

```
cd ..
rm -r magic
```


## Quick start

The easiest way to test MaGIC is to run the Shiny app (see [Tutorial](https://github.com/gimelbrantlab/magic/blob/master/Tutorial.md) for help). 
To run MaGIC in command you will run the following commands.

*Process.R*

Process.R script will take ChIP-seq input files in bigWig format and convert them into informative tables primed for analyze.R or generate.R. To run process.R for a test user case (human GM12787, chr15, H3k27me3 and H3k36me3 marks), run this command:

```
Rscript src/process.R -i data/input.txt -o data/output -r "hg19" -p 0 -s 3

```
You will get joint_scores_percentile.txt in your output folder, and this file will be used for further analysis.

*Generate.R*

Generate.R script will train model using pre-processed data from joint_scores_percentile_full_dataset.txt file packaged with installation. Run the following command to train ada, SVM and random forest model using 80% percent of the data for training and 20% for testing:

```
Rscript src/generate.R -i data/joined_scores_percentile.txt -o data/classifiers -a "human" -p 80 -l ada,svmPoly,rf -c 5
```
The output folder will contain model_output folder with summary_models.tsv file describing key metrics of the models' performance. 

*Analyze.R*

Finally, you will call analyze.R, using the joined_scores_percentile.txt (output from process.R) and the default classifiers that are packaged with MaGIC 2.0 as inputs.
```
Rscript src/analyze.R -i data/joined_scores_percentile.txt -o data/output -m models
```
You can also run analyze.R with your own trained classifers, but be careful to avoid overfitting. 

## Contact us

Questions, comments and concerns can be directed to [Alexander Gimelbrant](alexander_gimelbrant@dfci.harvard.edu), [Sebastien Vigneau](Sebastien_Vigneau@dfci.harvard.edu), and [Svetlana Vinogradova](Svetlana_Vinogradova@dfci.harvard.edu). Additional inquiries about how the program works can be directed to [Henry Ward](henry.neil.ward@gmail.com) and [Sachit Saksena](sachitdsaksena@utexas.edu).
