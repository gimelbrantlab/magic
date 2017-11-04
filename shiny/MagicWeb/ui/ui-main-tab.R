# Copyright (C) 2017 Dana-Farber Cancer Institute Inc.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Questions, comments and concerns can be directed to
#   Alexander Gimelbrant: alexander_gimelbrant@dfci.harvard.edu
#   Sebastien Vigneau: Sebastien_Vigneau@dfci.harvard.edu
#   Svetlana Vinogradova: Svetlana_Vinogradova@dfci.harvard.edu
#   Henry Ward: henry.neil.ward@gmail.com
#   Sachit Saksena: sachitdsaksena@utexas.edu

###########
# MAIN TAB
###########


tabPanel(value = "main_panel",
         # title = p("MaGICs", 
         #          style = "font-size: 20px; padding-bottom: -0.5cm"),
         title = icon(name = "magic", class = "fa-2x", lib = "font-awesome"),
         fluidRow(column(12,
         sidebarLayout(
           mainPanel(
             h2(HTML("Welcome to MaGICs: <br> <b>M</b>ono<b>a</b>llelic <b>G</b>ene <b>I</b>nference from <b>C</b>hromatin for <b>s</b>hiny")),
             actionButton("get_started", "Get started"),
             br(),
             h3(HTML("<u>Overview</u>")),
             tags$p(HTML(
               "MaGIC 2.0 is a graphical user interface for predicting allele specific expression in
               polyclonal cell samples. The program leverages the predictive power of chromatin signatures
               to generate educated inferences on monoallelic state of genes within a polyclonal sample via
               statistical learning methods and monoclonal allelic bias determination.")),
             h3(
               HTML("MaGIC offers three distinct functionalities: <br>")
             ),
             h4(
               HTML("1. Process")
             ),
             tags$p(HTML("Process is a shiny wrapper for the command line utility <b>process.R</b>.
                         This program functions to convert bigWig data from processed ChIP-Seq reads and
                         convert them to mapped scores compatible with analysis of MAE status through
                         analyze.R and model generation through generate.R. Process directly passes the
                         bigWig files inputted to this page and outputs *.tsv* files with ChIP-Seq region
                         scores by gene. Navigate to the <b>Process</b> tab to use this feature.")),
             h4(
               HTML("2. Analyze")
             ),
             tags$p(HTML("Analyze is a shiny wrapper for the command line utility <b>analyze.R</b>.
                         This program functions to provide predictions for a sample using a
                         previously trained model to determine status for genes within that sample. Simply provide
                         a file of ChIP-Seq enrichment scores from process.R and retrieve predictions.
                         Navigate to the <b>Analyze</b> tab to use this feature.")),
             h4(
               HTML("3. Generate")
             ),
             tags$p(HTML("We encourage the use of MaGIC 2.0 to improve upon published predictive models
                         for inferring MAE to improve predictive reliability and indicate possible mechanistic
                         implications of epigenetic signatures in MAE. To this end, Generate is a shiny wrapper
                         for the command line utility <b>generate.R</b>. This program functions to allow users to
                         input training data based on monoclonal allelic imbalance data and novel epigenetic signature
                         data to create models with a variety of different algorithms to further optimize the effort to
                         characterize cell-cell allele-specific heterogeneity. Navigate to the <b>Model</b> tab to use
                         this feature.")),
             p(),
             h3(
               HTML("How to provide MaGIC with your data. <br>")
               ),
                h4(
                  HTML("Provide data to 'Process' tab.")
                ),
                 tags$p(HTML("<b>Files</b> takes a correctly formatted '.txt' or '.tsv' file with names of chromatin marks
                  and control files and automatically selects the correct files from the input directory."),
                        p(HTML("To use this method input a file following the format below with fields seperated by tabs
                     in a text file format:<br>
                               <table style='width:50%'>
                               <tr>
                               <th>mark name</th>
                               <th>mark file</th>
                               <th>control file</th>
                               </tr>
                               <tr>
                               <td>mark1</td>
                               <td>input_dir/sample_mark1.bigWig</td>
                               <td>input_dir/control_mark1.bigWig</td>
                               </tr>
                               <tr>
                               <td>mark2</td>
                               <td>input_dir/sample_mark2.bigWig</td>
                               <td>input_dir/sample_mark2.bigWig</td>
                               </tr>
                               </table>")),
                        br(),
                        p("Put this *.txt* or *.tsv* file in the directory with the *.bigWig* files you want to process.
                        The program will now automatically pick the filenames you provided when you
                          move to the processing tab."),
                        p("You then set the *output* 
                          directory of where the processed data tables will be stored.")
               ),
             h4(
               HTML("Provide data to 'Model' tab.")
             ),
             h5(
               HTML("Generating a new model:")
             ),
             tags$p(HTML("In the 'Model' tab, you can generate new models using processed marks. 
                         You can provide data from Process.R output by 
                         selecting the file named 'joined_scores_percentile' from the designated output folder. 
                         You then set an output folder for models that you generate and tables showing performance of models.")),
             tags$p(HTML("To validate the model with an external dataset, you can provide another table of processed marks and gene status.")),
             h5(
                HTML("Using an existing model:")
             ),
             tags$p(HTML("In the 'Model' tab, you can also use models packaged with the MaGIC download by selecting the model
                         from the menu in the 'Existing' tab in the Model section. You can also provide a model you generated
                         previously (in the form of an *.rds* file) by selecting the folder containing these models and picking
                         models from a dropdown menu.")),
             h4(
               HTML("Provide data to 'Analyze' tab.")
               ),
             tags$p(HTML("To use models to get predictions in the Analyze, first provide the dataset you
                         want predictions for. Then select an output directory for the predictions.
                         If you want to filter your predictions for expression and gene length, you can 
                         upload a file with the following format:
                          <table style='width:20%'>
                          <tr>
                         <th>gene</th>
                         <th>expression</th>
                         <th>length</th>
                         </tr>
                         <tr>
                         <td>A1BG</td>
                         <td>10</td>
                         <td>2400</td>
                         </tr>
                         <tr>
                         <td>A1CF</td>
                         <td>2400</td>
                         <td>2500</td>
                         </tr>
                         </table>"
                         )),
             tags$p(HTML("Then select the length of genes you want to filter.")),
             width = 12
             ),
           mainPanel(NULL)
           )
         #### File method
           )
         )
         )
