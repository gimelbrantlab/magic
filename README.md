# MaGIC

MaGIC is a tool to classify genes into monoallelically expressed (MAE) and biallelically expressed (BAE) using
chromatin mark enrichment data. It is primarily based on the work of the [Gimelbrant lab](https://gimelbrantlab.med.harvard.edu). We have a standalone MaGIC pipeline and also a Shiny app.
The method is published in [BMC Bioinformatics]https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-019-2679-7

## Docker

To get started, we suggest to use Docker image with the Shiny app for MaGIC. The only requirement is to have Docker installed. Please see the [instructions](https://github.com/gimelbrantlab/magic/blob/master/Installation.md) if you don't have it installed yet.

Now, open a terminal window and invoke the Docker program directly. Checking the version is always a good way to test that a program will run without investing too much effort into finding a command that will work, so let's do:
```
docker --version
```
This should return something like "Docker version 18.03.1-ce, build 9ee9f40"

Still in your terminal (it doesn't matter where your working directory is), run the following command to retrieve the MaGIC image from Docker Hub:
```
docker pull kintany/shinymagic:current
```
To run a container you will use the following command:
```
docker run --rm -p 3838:3838 kintany/shinymagic:current
```
This will run the container and you need to open http://0.0.0.0:3838/ in any browser (Windows users should use "http://localhost:3838/" instead). Enjoy!

To be able to analyze your own ChIP-seq data and save the results, you need to mount a directory with the data: add -v parameter to do this. For example, to mount `~/Documents/data/` you will need to run a container like this:

```
docker run --rm -v ~/Documents/data/:/root/data -p 3838:3838 kintany/shinymagic:current
```
To stop the running container you need to get its id with the command:
```
docker container ls
```
and then use this id to stop the container:
```
docker stop 30b679c9c448
```

To get a better understanding how to use Shiny app and Magic in general, please go to "Help" tab or check out the [Documentation](https://github.com/gimelbrantlab/magic/blob/master/documentation.md). 

## Direct installation

### Requirements

If you don't want to use Docker, follow these instructions.

Linux/Mac machine with updated R (3.5.0 or later) and git. Please see the [instructions](https://github.com/gimelbrantlab/magic/blob/master/Installation.md) if you don't have git or R installed. There is a number of R packages that need to be installed, please see [dependencies](https://github.com/gimelbrantlab/magic/blob/master/Dependencies.md). During Magic installation, install.R script will try to download and install missing packages. However, if the installation of some of them fails, you will have to install them [manually](https://github.com/gimelbrantlab/magic/blob/master/Installation.md). 

### Installation

Installing MaGIC takes only a few commands. Required R packages and [bwtool](https://github.com/CRG-Barcelona/bwtool/wiki) are installed automatically.

To install MaGIC system-wide (i.e. with root/sudo access): 
```
git clone https://github.com/gimelbrantlab/magic.git
cd magic
Rscript src/install.R
```

To install MaGIC locally (i.e. on a server without root access to your R installation): 
```
git clone https://github.com/gimelbrantlab/magic.git
cd magic
Rscript src/install.R libraries
```
These commands will enable MaGIC to run from the command line. We also developed a Shiny app that can be run with the following command: 

```
R -e "shiny::runApp('shiny/MagicWeb/', launch.browser = T)"
```
Alternatively, if MaGIC has been installed locally as instructed above, use the following command instead:

```
R -e ".libPaths('libraries'); shiny::runApp('./shiny/MagicWeb/', launch.browser = T)"
```

If you change your mind and want to uninstall MaGIC, you can just delete the whole folder:

```
cd ..
rm -r magic
```


### Quick start

The easiest way to test MaGIC is to run the Shiny app (see [Tutorial](https://github.com/gimelbrantlab/magic/blob/master/shiny/MagicWeb/Tutorial.md) for help).

The following commands show you how to run MaGIC from the command line.

*Process.R*

Process.R script takes ChIP-seq files in bigWig format and converts them into normalized enrichment over genomic intervals (gene body or promoter), formatted as tables usable by analyze.R or generate.R. To run process.R using test data (human GM12787, chr15, H3K27me3 and H3K36me3 marks), run this command:

```
Rscript src/process.R -i data/input.txt -o data/output -r "hg19" -p 0 -s 3

```
You will get joint_scores_percentile.txt in your output folder. To test other parts of the pipeline, we suggest to use joined_scores_percentile_GM12878.txt file which contains the full dataset.

*Generate.R*

Generate.R script trains models using output from process.R saved in the joined_scores_percentile.txt file. We also provide a pre-generated joined_scores_percentile_GM12878.txt file. Run the following command to train ada, SVM, and random forest model using 80% percent of the data for training and 20% for testing:

```
Rscript src/generate.R -i data/joined_scores_percentile_GM12878.txt -o data/classifiers -a "human" -p 80 -l ada,svmPoly,rf -c 5
```
The output folder will contain a model_output folder containing a summary_models.tsv file describing key metrics of the models' performance. 

*Analyze.R*

Finally, call analyze.R, using the joined_scores_percentile.txt (output from process.R) and the default classifiers that are packaged with MaGIC as inputs.
```
Rscript src/analyze.R -i data/joined_scores_percentile_GM12878.txt -o data/output -m models
```
You can also run analyze.R with your own trained classifers, but be careful to avoid overfitting. 

## Citation

Please cite the following paper if using Magic:
Vinogradova S, Saksena SD, Ward HN, Vigneau S, Gimelbrant AA. MaGIC: a machine learning tool set and web application for monoallelic gene inference from chromatin. BMC Bioinformatics. 2019 Feb 28;20(1):106.

## Contact us

Questions, comments and concerns can be directed to [Alexander Gimelbrant](alexander_gimelbrant@dfci.harvard.edu), [Sébastien Vigneau](Sebastien_Vigneau@dfci.harvard.edu), and [Svetlana Vinogradova](Svetlana_Vinogradova@dfci.harvard.edu). Additional inquiries about how the program works can be directed to [Henry Ward](wardx596@umn.edu) and [Sachit Saksena](sachitdsaksena@utexas.edu).
