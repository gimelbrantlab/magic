
##########
# ANALYZE TAB
##########



tabPanel(value = "analyze",
         title = ">Analyze",
         sidebarLayout(
           sidebarPanel(
             fileInput('analysisFile', 'Upload processed TSV file',
                       accept = acceptable_file_types
                       ),
             selectizeInput(
               "positiveClass",
               "Select positive class",
               choices = positive_classes,
               selected = "MAE"
               ),
             conditionalPanel(
               condition = "input.positiveClass == other",
               textInput("positiveClass",
               label = "Enter positive class")
              ),
             actionButton("analyzeDataButton", "Analyze data", width = "100%")
           ),
           mainPanel(
             htmlOutput("analysisText"),
             conditionalPanel(condition = "output.analyze_output",
                              downloadButton("downloadAnalyzeButton",
                                             "Download analysis"))
           ))
)