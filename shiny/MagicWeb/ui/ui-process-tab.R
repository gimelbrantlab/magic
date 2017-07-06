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