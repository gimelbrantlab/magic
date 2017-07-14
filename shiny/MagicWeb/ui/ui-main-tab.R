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
         title = p("MaGICs", style = "font-size: 20px; padding-bottom: -0.5cm"),
         fluidRow(column(12,
         sidebarLayout(
           sidebarPanel(
             h2(HTML("Welcome to MaGICs: <br> <b>M</b>ono<b>a</b>llelic <b>G</b>ene <b>I</b>nference from <b>C</b>hromatin for <b>S</b>hiny")),
             actionButton("get_started", "Get started"),
             br(),
             h2(HTML("<u>Overview</u>")),
             tags$p(HTML(
               "MaGIC 2.0 is a graphical user interface for predicting allele specific expression in
               polyclonal cell samples. The program leverages the predictive power of chromatin signatures
               to generate educated inferences on monoallelic state of genes within a polyclonal sample via
               statistical learning methods and monoclonal allelic bias determination.")),
             p(),
             h4(
               HTML("Now, choose a format to provide MaGIC with your data. <br>")
               ),
                 tags$p(HTML("<b>Files</b> takes a correctly formatted '.txt' or '.tsv' file with names of chromatin marks
                  and control files and automatically selects the correct files from the input directory. <br><br>
                  <b>Fields</b> creates fields for inputting mark names into the interface.")
               ),
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
                         scores by gene. Navigate to the <b>Processing</b> tab to use this feature.")),
             h4(
               HTML("2. Analyze")
             ),
             tags$p(HTML("Analyze is a shiny wrapper for the command line utility <b>analyze.R</b>.
                     This program functions to provide predictions for a sample using a
                     previously trained model to determine status for genes within that sample. Simply provide
                     a file of ChIP-Seq enrichment scores from process.R and retrieve predictions.
                     Navigate to the <b>Analysis</b> tab to use this feature.")),
             h4(
               HTML("3. Generate")
             ),
             tags$p(HTML("We encourage the use of MaGIC 2.0 to improve upon published predictive models
                         for inferring MAE to improve predictive reliability and indicate possible mechanistic
                         implications of epigenetic signatures in MAE. To this end, Generate is a shiny wrapper
                         for the command line utility <b>generate.R</b>. This program functions to allow users to
                         input training data based on monoclonal allelic imbalance data and novel epigenetic signature
                         data to create models with a variety of different algorithms to further optimize the effort to
                         characterize cell-cell allele-specific heterogeneity. Navigate to the <b>Model Generation</b> tab to use
                         this feature.")),
             width = 12
             ),
           mainPanel(img(src='opening_slide.png', align='left', width='900px', height='530px')),
           position = "left"
           )
         #### File method
           )
         )
         )