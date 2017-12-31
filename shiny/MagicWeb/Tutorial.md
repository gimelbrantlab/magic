## Tutorial 

This tutorial will walk you through the steps of using Shiny app

### Step 1. ChIP-seq data processing

First step is to process your ChIP-seq data from bigWig format to table format ready for use by generate.R and analyze.R. To do this, we need to specify path to the folder containing your data and also input.txt file (see [documentation](https://github.com/gimelbrantlab/magic/blob/master/documentation.md) for input.txt file format). For your convenience, we included testing data in data folder, so your can run this example. You also need to specify output folder and couple parameters for processing. Please specify assembly and if you want to include promoters into analysis, please specify their length. You can also choose filters to remove certain groups of genes from the analysis. 

<img src="www/images/Step1.0.png" alt="Step1.0" width="400"/>

In most cases, we want to filter out genes with input (control) values below certain threshold because these genes can give a very noisy signal. To do this, you can select the threshold for input values.

#### Output

The output consists of several parts. File joined_scored_percentile.txt will be saved in your output folder, and will be used for further analysis. Images displayed at this tabs are used for quality control and to understand how your data looks like.

*Processed data table* tab is the table representation of the file joined_scored_percentile.txt. You can look up genes of interest here, and also sort your data to make sure you don't have any unusual values like Inf (hopefully, you don't!). 

<img src="www/images/Step1.2.png" alt="Step1.1" width="400"/>

*ChIP QC* tab provides insight into your ChIP-seq data. If you are going to classify genes into monoallelic and biallelic, it is expected that all genes' ChIP-seq values distributions are bimodal or at least close to bimodal. Otherwise, you won't be able to get a meaningful classification. When you make a scatterplot for one chromatin mark versus another you also expect to have two lobes representing two groups of genes. If you don't, maybe you should reconsider if this data is appropiate for the analysis. 

<img src="www/images/Step1.3.png" alt="Step1.2" width="400"/>

*Input distribution* tab contains the plot with distribution of values in your input file. Also, a good QC check and you can use it to decide about the lowest threshold for your input.

<img src="www/images/Step1.4.png" alt="Step1.3" width="400"/>

### Step 2. Model generation

The second step is to train a model. If you don't want to generate a new model, you can skip this step and use one of our models at the next step. 
To train models, please specify the path to joined_scores_percentile.txt generated at the previous step (or, if you just want to run a test case, select joined_scores_percentile_full_dataset.txt from the data folder). You should also specify classes of models you want to train and what dataset of genes with know statuses should be uses for training (select betweem our human or mouse datasets, or specify our own). For additional parameters, please see [documentation](https://github.com/gimelbrantlab/magic/blob/master/documentation.md).

<img src="www/images/Step2.0.png" alt="Step2.0" width="400"/>

#### Output

Trained models are tested with 20% of the data (this percent can be changed) and models' performance is summarized here. 

<img src="www/images/Step2.1.png" alt="Step2.1" width="700"/>

<img src="www/images/Step2.2.png" alt="Step2.2" width="400"/>

### Step 3. Classification

The last step is to make predictions. Use joined_scores_percentile.txt file to make predictions with model in the folder that you specify. If you want to use our models, don't change the folder; if you want to use your own models, specify the folder with models.

<img src="www/images/Step3.0.png" alt="Step3.0" width="400"/>

#### Output

The output predictions are saved in analysis_output folder for you. This tab gives you a table representation and allows to easily search for genes of interest.

<img src="www/images/Step3.1.png" alt="Step3.1" width="600"/>

