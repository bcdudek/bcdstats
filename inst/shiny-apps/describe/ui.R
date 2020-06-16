# UI for app
shinyUI(pageWithSidebar(
  # title
  headerPanel("Univariate Data Description and Visualization"),
  #input
  sidebarPanel
  (
    selectInput("dataset","Choose Data Set (see the 'Data Sets' tab):",
                list("Aron Table 1-5" = "aron1_5", 
                     "Mouse Alcohol Drinking" = "mouse1",
                     "Morning Person Rating" = "morning1",
                     "Old Faithful" = "faithful2")
    ),
    selectInput("variable","Choose Variable:", "Loading..."),
  #    uiOutput("variable"), # depends on dataset ( set by output$variable in server.R)
    selectInput("plottype","Choose Plot Type:",
                list(Boxplot = "boxplot",
                    "Frequency Histogram" = "histogram",
                    "Equal Area Histogram"= "eahist",
                    "Diagonally Cut Histogram" = "dchist",
                    "QQ Normal Plot" = "qqp",
                    "Violin Plot" = "violin"
                     )
    ),
    
   conditionalPanel(condition = "input.plottype == 'histogram'",
                    wellPanel(
                    selectInput(
                      "breaks", "Bin Width Adjustment Method",
                      c("Sturges Method"="Sturges",
                        "[Custom Number of Bins]" = "custom",
                        "Scott Method"="Scott",
                        "Freedman-Diaconis Method"="Freedman-Diaconis"
                        )),
                    
                    # Only show this panel if Custom is selected
                    conditionalPanel(
                      condition = "input.breaks == 'custom'",
                      sliderInput("breakCount", "Adjust Number of Bins (not always precise) ", min=1, max=250, value=10)
                    ),          
   checkboxInput(inputId = "density",
                  label = strong("Show kernel density estimate"),
                  value = FALSE),
   conditionalPanel(condition = "input.density", 
                    uiOutput("sliderSetup"),
#             sliderInput(inputId = "bw_adjust",
#                       label = "Kernel Density Bandwidth Adjustment:",
#                       min = 0.02, max = 10, value = 1, step = 0.01),
             verbatimTextOutput("ddx2"),
             htmlOutput("kde")
                    )
                    )
     ),
    htmlOutput("gtype")
      ),
  
  # output
  mainPanel(
    tabsetPanel(
      tabPanel("Plots/Summary",
        h3(textOutput("caption")),
       # depends on input
#        uiOutput("plot"),
        plotOutput("p"),
               wellPanel(p(strong("Descriptive Statistics")),
               tableOutput("tab1"))),
              #    verbatimTextOutput("d"))),
      tabPanel("Data Sets",
               includeMarkdown('aboutdata.md')),
      tabPanel("About", 
               includeMarkdown('about.md'))
    ))  
))
        
