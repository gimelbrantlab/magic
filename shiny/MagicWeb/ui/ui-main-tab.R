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
         title = icon(name = "magic", class = "fa-2x", lib = "font-awesome"),
         fluidRow(column(12,
                         sidebarLayout(
                           mainPanel(
                             h2(HTML("<b>M</b>ono<b>a</b>llelic <b>G</b>ene <b>I</b>nference from <b>C</b>hromatin")),
                             actionButton("get_started", "Get started"),
                             br(),
                             h4(HTML("<u>Overview</u>")),
                             tags$p(HTML(
                               "A large fraction of human and mouse genes is subject to clonally-variable allelic expression imbalance, generally designated as monoallelic gene expression (MAE). MAE is thought to play a role in various biological processes including cancer, haploinsufficiency, variable expressivity of disease, and embryonic development. Direct detection of MAE in primary cells and tissues is challenging but MAE can be accurately inferred using histone methylation data in a way that does not require polymorphisms. The MaGIC tool allows MAE inference in a user friendly manner through a web application or from the command line.")),
                             h4(HTML("<u>The pipeline</u>")),
                             tags$p(HTML(
                               "Process.R calculates ChIP-seq enrichment per transcript from ChIP-seq and control bigWig files.
               Generate.R trains classifiers using ChIP-seq enrichment and true MAE calls.
               Analyze.R uses classifiers to predict MAE from ChIP-seq data."
                             )),
                             tags$img(HTML("<img src = \"images/pipeline_Magic.png\", height = 192, width = 800>")),
                             width = 12
                           ),
                           mainPanel(NULL)
                         )
         )
         )
)
