#!/bin/bash
#June 2016, Sachit Saksena
##This file converts SRA -> FASTQ -> gene_expression.csv
##REQUIRES SRA TOOLKIT AND RSEM INSTALLED AS SRATOOLKIT AND RSEM_TOOLS, RESPECTIVELY
##ALSO REQUIRES BOWTIE INSTALLED AS NAMED FROM BOWTIE WEBSITE
#install sratoolkit and put in directory called sratoolkit and rsem in rsem_tools

#sra_file= file or path containing "<chromatin_mark><rep>.sra
#output_fastq=<chromatin_mark><rep>.fastq
#rsem_chromosome_path= path to directory containing all chromosome sequences
#rsem_reference_gtf == file or path containing reference .gtf file mm9.gtf
#sample_name == name or path of all reference outputs
#final=<chromatin_mark><rep>.csv


## get params from external file
if [ -z "$1" ]
	then
	echo "Usage $0 <Param.file>"
	exit
fi

## Reading parameter from the Parameters file #########################################
param_file=$1

#checks for valid .sra file
sra_file=$(grep -w "^sra_file"  "$param_file" | sed 's/.*=//' | sed 's/\s+$//')
if [ ! $sra_file ]
then
echo "$0: No sra file specified $sra_file"
	exit
fi

output_fastq=$(grep -w "^output_fastq"  "$param_file" | sed 's/.*=//' | sed 's/\s+$//')
if [ ! $output_fastq ]
then
	echo "$0: No fastq output specified $output_fastq"
	exit
fi

################################ RSEM PARAMETERS ###########################################

## get params from external file 
rsem_reference_gtf=$(grep -w "^rsem_reference_gtf"  "$param_file" | sed 's/.*=//' | sed 's/\s+$//')
if [ ! $rsem_reference_gtf ]
then
	echo "$0 No refseq gtf file provided $rsem_reference_gtf"
	exit
fi

rsem_chromosome_path=$(grep -w "^rsem_chromosome_path" "$param_file" | sed 's/.*=//' | sed 's/\s+$//')
if [ ! $rsem_chromosome_path ]
then
	echo "$0 No chromosome path provided $rsem_chromosome_path"
	exit
fi

sample_name=$(grep -w "^sample_name" "$param_file" | sed 's/.*=//' | sed 's/\s+$//')
if [ ! $sample_name ]
then
       	echo "$0 No sample_name for output provided $sample_name"
	exit
fi

final_output=$(grep -w "^final_output" "$param_file" | sed 's/.*=//' | sed 's/\s+$//')
if [ ! $final_output }
then
        echo "$0 No final_output specified $final_output"
        exit
fi

echo ">>got params<<"
echo "..."
echo "..."
echo "..."
echo "..."
echo "..."
#read in 
echo "Converting sra to fastq..."

#for job submission
#echo "Submitting batch job..."
#bsub -n 4 -W 03:00 -q short ./sratoolkit/bin/fastq_dump $sra_file > $output_fastq

#for interactive system 

./sratoolkit/bin/fastq-dump -Z $sra_file > $output_fastq

echo ">>preparing references<<"
echo "Submitting batch job..."
#create reference
#bsub -n 8 -W 03:00 -q short ./rsem_tools/rsem-prepare-reference --gtf $rsem_reference_gtf --bowtie --bowtie-path ./bowtie-1.1.2 --bowtie2 --bowtie2-path ./bowtie2-2.2.9 ./chr $sample_name
#interactive session

./rsem_tools/rsem-prepare-reference --gtf $rsem_reference_gtf --bowtie --bowtie-path ./bowtie-1.1.2 --bowtie2 --bowtie2-path ./bowtie2-2.2.9 ./chr $sample_name


echo ">calculating expression<"
echo "..."
echo "..."
#calculate gene expression
#bsub -n 8 -W 03:00 -q short ./rsem_tools/rsem-calculate-expression \
#interactive sesssion

./rsem_tools/rsem-calculate-expression --bowtie2 --bowtie2-path ./bowtie2-2.2.9 --append-names --estimate-rspd -p 8 --output-genome-bam $output_fastq $sample_name expression_output

