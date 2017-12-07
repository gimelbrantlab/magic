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
             h2(HTML("Welcome to MaGIC: <br> <b>M</b>ono<b>a</b>llelic <b>G</b>ene <b>I</b>nference from <b>C</b>hromatin")),
             actionButton("get_started", "Get started"),
             br(),
             h4(HTML("<u>Overview</u>")),
             tags$p(HTML(
              "A large fraction of human and mouse genes is subject to a regulatory mechanism that controls 
              allele-specific and <b> monoallelic expression (MAE)</b>. MAE detection is challenging; at the same time, 
              it is highly tissue-specific, and being able to detect it in multiple cell and tissue types is 
              necessary to understand and map it.  We have previously reported that a specific and sensitive 
              gene-body <b>chromatin signature</b> can identify MAE genes in a sequence-independent way in 
              multiple tissue types. Here we present a robust and convenient pipeline for <b>monoallelic gene 
              inference from chromatin (MaGIC)</b>. It uses histone mark signature to 
              predict monoallelic versus biallelic gene expression and can be used via command line or 
              Shiny web application.")),
             h4(HTML("<u>The pipeline</u>")),
             tags$p(HTML(
               "Process.R calculates ChIP-seq enrichment per transcript from ChIP-seq and control bigWig files. 
               Generate.R trains classifiers using ChIP-seq enrichment and true MAE/BAE calls. 
               Analyze.R uses classifiers to predict MAE/BAE gene status from ChIP-seq data. "
             )),
             tags$img(HTML("<img src = \"images/pipeline_Magic.pdf\", height = 384, width = 512>")),
             #tags$p(img(src = "images/pipeline_Magic.pdf")),
             width = 12
             ),
           mainPanel(NULL)
           )
         #### File method
           )
         )
         )
