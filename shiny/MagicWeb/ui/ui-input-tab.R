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

############
# INPUT TAB
############


tabPanel(value = "input",
    title = "Input data",
  sidebarLayout(
    sidebarPanel(
      br(),
      h4( HTML("First, set your working directory where your data lives.")),
      directoryInput("dataDirectory",
                     label = "Select input directory:"),
      br(),
      h4(HTML("Next, set your output folder for output from process.R, analyze.R, and generate.R")),
      directoryInput("outputDirectory",
                     label = "Select output directory"),
     width = 8
    ),
    mainPanel(
    fluidRow(column(12,
         tabsetPanel(
           tabPanel("Option 1: File",
                    column(12,
                           sidebarLayout(
                             fluidRow(
                               mainPanel(
                                 tags$h3(HTML("<u>Input to Processing</u>")),
                                 p(HTML("To use this method input a file following the format below with fields seperated by tabs
                                        in *.txt* or *.tsv* format:<br>
                                        <table style='width:100%'>
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
                                 p("The program will now automatically pick the filenames you provided when you
                                   move to the processing tab."),
                                 width = 12
                                 )
                                 ),
                             fluidRow(
                               sidebarPanel(
                                 fluidRow(
                                   textInput(
                                   'fileInput',
                                   label = h5("Upload file"),
                                   value = NULL
                                 )),
                                 width = 12
                               )
                             ))))
  
           ### Field method
           # tabPanel("Option 2: Fields",
           #          column(12,
           #                 sidebarLayout(
           #                   fluidRow(
           #                     mainPanel(
           #                       tags$h3(HTML("<u>Input usage</u>")),
           #                       p(HTML("To use this method, first indicate how many mark files you are evaluating,
           #                              then, simply fill in the following fields with the exact file names
           #                              of the unprocessed chromatin signature data in the provided directory.")),
           #                       width = 12
           #                       )
           #                       ),
           #                   fluidRow(
           #                     sidebarPanel(
           #                       fluidRow(numericInput(
           #                         "marks",
           #                         "Number of epigenetic marks being evaluated:", value=0,
           #                         min = 2, max = 5
           #                       ),
           #                       conditionalPanel(condition = "input.marks == 2",
           #                                        textInput("filename1", "Mark file name of first mark", "mark1_sample.bigWig"),
           #                                        textInput("control1", "Control file name of first mark", "mark1_control.bigWig"),
           #                                        textInput("filename2", "Mark file name of second mark", "mark2_sample.bigWig"),
           #                                        textInput("control2", "Control file name of second mark", "mark2_control.bigWig")
           #                       ),
           #                       conditionalPanel(condition = "input.marks == 3",
           #                                        textInput("filename1", "Mark file name of first mark", "mark1_sample.bigWig"),
           #                                        textInput("control1", "Control file name of second mark", "mark1_control.bigWig"),
           #                                        textInput("filename2", "Mark file name of second mark", "mark2_sample.bigWig"),
           #                                        textInput("control2", "Control file name of second mark", "mark2_control.bigWig"),
           #                                        textInput("filename3", "Mark file name of third mark", "mark3_sample.bigWig"),
           #                                        textInput("control3", "Control file name of third mark", "mark3_control.bigWig")
           #                       ),
           #                       conditionalPanel(condition = "input.marks == 4",
           #                                        textInput("filename1", "Mark file name of first mark", "mark1_sample.bigWig"),
           #                                        textInput("control1", "Control file name of second mark", "mark1_control.bigWig"),
           #                                        textInput("filename2", "Mark file name of second mark", "mark2_sample.bigWig"),
           #                                        textInput("control2", "Control file name of second mark", "mark2_control.bigWig"),
           #                                        textInput("filename3", "Mark file name of third mark", "mark3_sample.bigWig"),
           #                                        textInput("control3", "Control file name of third mark", "mark3_control.bigWig"),
           #                                        textInput("filename4", "Mark file name of fourth mark", "mark4_sample.bigWig"),
           #                                        textInput("control4", "Control file name of fourth mark", "mark4_sample.bigWig")
           #                       ),
           #                       conditionalPanel(condition = "input.marks == 5",
           #                                        textInput("filename1", "Mark file name of first mark", "mark1_sample.bigWig"),
           #                                        textInput("control1", "Control file name of second mark", "mark1_control.bigWig"),
           #                                        textInput("filename2", "Mark file name of second mark", "mark2_sample.bigWig"),
           #                                        textInput("control2", "Control file name of second mark", "mark2_control.bigWig"),
           #                                        textInput("filename3", "Mark file name of third mark", "mark3_sample.bigWig"),
           #                                        textInput("control3", "Control file name of third mark", "mark3_control.bigWig"),
           #                                        textInput("filename4", "Mark file name of fourth mark", "mark4_sample.bigWig"),
           #                                        textInput("control4", "Control file name of fourth mark", "mark4_sample.bigWig"),
           #                                        textInput("filename5", "Mark file name of fifth mark", "mark5_sample.bigWig"),
           #                                        textInput("control5", "Control file name of fifth mark", "mark5_sample.bigWig")
           #                       )
           #                       ),
           #                       width=12
           #                     )
           #                   )
          #              ))
         # ))
  )))
  )),
  fluidRow(
  actionButton("next_process", "Next"),
  tags$style(type="text/css", "#next_process { width:10%; margin-left: 1000px;}")
  )
  )
