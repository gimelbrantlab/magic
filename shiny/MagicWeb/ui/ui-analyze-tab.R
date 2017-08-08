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
             fileInput('analysisFile', 'Upload processed TSV file',
                       accept = acceptable_file_types
                       ),
             actionButton("analyzeDataButton", "Analyze data", width = "100%"),
             conditionalPanel(condition = "input.analyzeDataButton",
                              checkboxInput("expression_filter", label = "Filter lowly expressed genes?"),
                              conditionalPanel(condition = "input.expression_filter == TRUE",
                              fileInput('expressionData', label = "Input RNA-Seq file"))
             )
           ), mainPanel(
             print("Hello world")
           ) # end of overall mainPanel 
             
             
             # htmlOutput("analysisText"),
             # conditionalPanel(condition = "output.analyze_output",
             #                  downloadButton("downloadAnalyzeButton",
             #                                 "Download analysis"))
           )
           )


