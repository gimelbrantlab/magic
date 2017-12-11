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

##########
# ANALYZE TAB
##########

tabPanel(value = "analyze",
         title = "Analyze",
         sidebarLayout(
           sidebarPanel(
             tags$p(HTML("<br><b>Select output directory</b>")),
             shinyDirButton("analyzeOutput", "Output directory", "Upload"),
             verbatimTextOutput("analyzeOutput", placeholder = TRUE),
             #verbatimTextOutput("outputPath", placeholder = TRUE),
             br(),
             br(),
             fileInput('analysisFile', 'Upload file with processed chromatin data',
                       accept = acceptable_file_types
                       ) %>% 
               shinyInput_label_embed(
                 icon("info") %>%
                   bs_embed_tooltip(title = "In most cases, it is joined_scores_percentile.txt in your output folder")
               ),
             fileInput('expressionData', 
                       label = "Upload file with gene lengths and expression (if available)") %>% 
               shinyInput_label_embed(
                 icon("info") %>%
                   bs_embed_tooltip(title = "Please see readme for the file format")
               ),
             selectizeInput(
               "exModels",
               "Select models to exclude from analysis",
               choices = model_list,
               multiple=TRUE
             ) %>% 
               shinyInput_label_embed(
                 icon("info") %>%
                   bs_embed_tooltip(title = "If you want to exclude some (generated on the previous step) models from analysis, do it here")
               ),
             textInput("positiveClass", "Positive class:",
                       value = "MAE"),
             # checkboxInput("expression_filter", 
             #               label = "Filter genes by expression and/or length?",
             #               value = FALSE),
                              # conditionalPanel(
                              #   condition = "input.expression_filter == TRUE",
                              #   fileInput('expressionData', 
                              #     label = "Input RNA-Seq file")
                              # ),
             numericInput("lengthFilter", 
                         "Set length filter",
                         value=2500) %>% 
               shinyInput_label_embed(
                 icon("info") %>%
                   bs_embed_tooltip(title = "Set length lower threshold here, if you want to filter out shorter genes")
               ),
             actionButton("analyzeDataButton", "Analyze data", width = "100%"),
             br(),
             tags$p(HTML("<br><b>Now, to visualize your results, press the button below:</b>")),
             conditionalPanel("input.analyzeDataButton",
                              actionButton("analyzeTableButton", "Show table of prediction results", width = "100%")
             )
           ), mainPanel(tabsetPanel(id = "outputPlot",
                          tabPanel("Table",
                            dataTableOutput("predTbl")
           )
           ))
           )
)

