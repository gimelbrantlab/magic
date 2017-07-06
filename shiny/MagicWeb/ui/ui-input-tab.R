

############
# INPUT TAB
############


tabPanel(value = "input",
    title = "Input data",
  sidebarLayout(
    sidebarPanel(
      br(),
      h4( HTML("First, set your working directory that contains all your input data.")),
      directoryInput("processingDir",
                     label = "Select input directory:",
                     value = "~"),
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
                                 fluidRow(fileInput(
                                   'file_inputs',
                                   label = h5("Upload file"),
                                   accept = c(
                                     'text/comma-separated-values',
                                     'text/tab-separated-values',
                                     '.tsv'
                                   )
                                 )),
                                 width = 12
                               )
                             )))),
  
           ### Field method
           tabPanel("Option 2: Fields",
                    column(12,
                           sidebarLayout(
                             fluidRow(
                               mainPanel(
                                 tags$h3(HTML("<u>Input usage</u>")),
                                 p(HTML("To use this method, first indicate how many mark files you are evaluating,
                                        then, simply fill in the following fields with the exact file names
                                        of the unprocessed chromatin signature data in the provided directory.")),
                                 width = 12
                                 )
                                 ),
                             fluidRow(
                               sidebarPanel(
                                 fluidRow(numericInput(
                                   "marks",
                                   "Number of epigenetic signatures being evaluated:", value=0,
                                   min = 2, max = 5
                                 ),
                                 conditionalPanel(condition = "input.marks == 2",
                                                  textInput("filename1", "Mark file name of first mark", "mark1_sample.bigWig"),
                                                  textInput("control1", "Control file name of first mark", "mark1_control.bigWig"),
                                                  textInput("filename2", "Mark file name of second mark", "mark2_sample.bigWig"),
                                                  textInput("control2", "Control file name of second mark", "mark2_control.bigWig")
                                 ),
                                 conditionalPanel(condition = "input.marks == 3",
                                                  textInput("filename1", "Mark file name of first mark", "mark1_sample.bigWig"),
                                                  textInput("control1", "Control file name of second mark", "mark1_control.bigWig"),
                                                  textInput("filename2", "Mark file name of second mark", "mark2_sample.bigWig"),
                                                  textInput("control2", "Control file name of second mark", "mark2_control.bigWig"),
                                                  textInput("filename3", "Mark file name of third mark", "mark3_sample.bigWig"),
                                                  textInput("control3", "Control file name of third mark", "mark3_control.bigWig")
                                 ),
                                 conditionalPanel(condition = "input.marks == 4",
                                                  textInput("filename1", "Mark file name of first mark", "mark1_sample.bigWig"),
                                                  textInput("control1", "Control file name of second mark", "mark1_control.bigWig"),
                                                  textInput("filename2", "Mark file name of second mark", "mark2_sample.bigWig"),
                                                  textInput("control2", "Control file name of second mark", "mark2_control.bigWig"),
                                                  textInput("filename3", "Mark file name of third mark", "mark3_sample.bigWig"),
                                                  textInput("control3", "Control file name of third mark", "mark3_control.bigWig"),
                                                  textInput("filename4", "Mark file name of fourth mark", "mark4_sample.bigWig"),
                                                  textInput("control4", "Control file name of fourth mark", "mark4_sample.bigWig")
                                 ),
                                 conditionalPanel(condition = "input.marks == 5",
                                                  textInput("filename1", "Mark file name of first mark", "mark1_sample.bigWig"),
                                                  textInput("control1", "Control file name of second mark", "mark1_control.bigWig"),
                                                  textInput("filename2", "Mark file name of second mark", "mark2_sample.bigWig"),
                                                  textInput("control2", "Control file name of second mark", "mark2_control.bigWig"),
                                                  textInput("filename3", "Mark file name of third mark", "mark3_sample.bigWig"),
                                                  textInput("control3", "Control file name of third mark", "mark3_control.bigWig"),
                                                  textInput("filename4", "Mark file name of fourth mark", "mark4_sample.bigWig"),
                                                  textInput("control4", "Control file name of fourth mark", "mark4_sample.bigWig"),
                                                  textInput("filename5", "Mark file name of fifth mark", "mark5_sample.bigWig"),
                                                  textInput("control5", "Control file name of fifth mark", "mark5_sample.bigWig")
                                 )
                                 ),
                                 width=12
                               )
                             )
                           ))))
  )))
  ),
  fluidRow(
  actionButton("next_process", "Next"),
  tags$style(type="text/css", "#next_process { width:10%; margin-left: 1000px;}")
  )
  )