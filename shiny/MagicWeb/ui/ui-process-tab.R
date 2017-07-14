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

########
# PROCESS.R
########

tabPanel(value = "process",
        title = ">Process",
         sidebarLayout(
           sidebarPanel(
             selectizeInput(
               'organism', 'Select organism',
               choices = organism,
               selected = "human"
             ),
             conditionalPanel(
               condition = "input.organism == 'mouse'",
               selectizeInput('mouse_refseq', 'Select assembly',
                              choices = mouse_refseq_reference,
                              selected = "mm9")
               ),
             conditionalPanel(
               condition = "input.organism == 'human'",
               selectizeInput("human_refseq", 'Select assembly',
                              choices = human_refseq_reference,
                              selected = "hg19")
             ),
             conditionalPanel(
               condition = "input.organism == 'other'",
               fileInput(
                 'file1',
                 label = h5("Upload file"),
                 accept = c(
                   'text/comma-separated-values',
                   'text/tab-separated-values',
                   '.tsv'
                 )
               )
             ),
             selectizeInput(
               'tg', 'Select training genes',
               choices = tg_names,
               selected = "none"
             ),
             sliderInput("dropPercent", "Drop percent:", 0.01,
                          min = 0, max = 0.80, step = 0.01),
             numericInput("promoterLength", "Promoter Length:", 5000, min = 0,
                          step = 100),
             checkboxInput("disableFilter",
                           "Disable filtering?",
                           value = FALSE),
             checkboxInput("noOverlap",
                           "Disable overlap calculation?",
                           value = FALSE),
             actionButton("processDataButton", "Process data", width = "100%")
             ),
           mainPanel(
             htmlOutput("processText"),
             conditionalPanel(condition = "output.process_output",
                              downloadButton("downloadProcessButton",
                                             "Download processed data"))
             )
           ),
      fluidRow(column(12,
      actionButton("next_generate", "Next"),
      tags$style(type="text/css", "#next_generate { width:10%; margin-left: 1000px;}")
      )
      )
)