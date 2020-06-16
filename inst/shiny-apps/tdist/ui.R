# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Tail probability of Student's t Distribution"),
  
  # Sidebar with controls
  sidebarPanel(
    selectInput(
       "control", "Choose Type of Control:",
       c("Adjust Probability" = "prob", "Adjust X-axis quantile (t) value"="quant")
       ),
    numericInput("df",
                 "Degrees of Freedom",
                 min=1,
                 max=1000,
                 value=20),
    selectInput("tail","Which Tail of Distribution?",
                list("one-tailed upper" = "upper", 
                     "one-tailed lower" = "lower", 
                     "two-tailed" = "two")),    
    radioButtons("entrymode", "Use slider or direct text entry:",
                 c("Slider" = "slider",
                   "Number Entry Box" = "box")),
    
    conditionalPanel(
     condition="input.control=='prob'",
     conditionalPanel(
       condition="input.entrymode=='slider'",
     conditionalPanel(
       condition="input.tail=='upper'",
         sliderInput("pru", 
                "Tail Probability:", 
                min = .00001,
                max = .50, 
                value = .050,
                step=.001,sep="")),
     conditionalPanel(
       condition="input.tail=='lower'",
       sliderInput("prl", 
                 "Tail Probability:", 
                 min = .00001,
                 max = .50, 
                 value = .050,
                 step=.001,sep="")),
     conditionalPanel(
       condition="input.tail=='two'",       
       sliderInput("prt", 
                 "Tail Probability:", 
                 min = .00001,
                 max = 1.0, 
                 value = .025,
                 step=.001,sep=""))
     ),
     conditionalPanel(
       condition="input.entrymode=='box'",
       conditionalPanel(
         condition="input.tail=='upper'",
         numericInput("pvalu", "Enter Probability:", 
                      value=.05,
                      min = 0, 
                      max = 1.00,
                      step=.001)),
       conditionalPanel(
         condition="input.tail=='lower'",
         numericInput("pvall", "Enter Probability:", 
                      value=.05,
                      min = 0, 
                      max = 1.00,
                      step=.001)),
       conditionalPanel(
         condition="input.tail=='two'",       
         numericInput("pvalt", "Enter Probability:", 
                      value=.05,
                      min = 0, 
                      max = 1.00,
                      step=.001))
     )
     ),
    
    
    conditionalPanel(
      condition="input.control=='quant'",
            conditionalPanel(
              condition="input.tail=='upper'",
              conditionalPanel(
                condition="input.entrymode=='slider'",
                sliderInput("quantileu", 
                          "X-Axis Quantile (t) Value", 
                          min = 0,
                          max = 5.3, 
                          value = 2.00,
                          step=.001,sep="")),
              conditionalPanel(
                condition="input.entrymode=='box'",
                numericInput("quantileub", "Enter X Axis Quantile (t) Value:", 
                             value=2.00,
                             min = -5.3, 
                             max = 5.3,
                             step=.001))
              ),
      
      conditionalPanel(
        condition="input.tail=='lower'",
        conditionalPanel(
          condition="input.entrymode=='slider'",
          sliderInput("quantilel", 
                      "X-Axis Quantile (t) Value", 
                      min = -5.3,
                      max = 0, 
                      value = -2.00,
                      step=.001,sep="")),
        conditionalPanel(
          condition="input.entrymode=='box'",
          numericInput("quantilelb", "Enter X Axis Quantile (t) Value:", 
                       value=-2.00,
                       min = -5.3, 
                       max = 5.3,
                       step=.001))
      ),
      
      conditionalPanel(
        condition="input.tail=='two'",
        conditionalPanel(
          condition="input.entrymode=='slider'",
          sliderInput("quantilet", 
                      "X-Axis Quantile (t) Value", 
                      min = 0,
                      max = 5.3, 
                      value = 2.00,
                      step=.001,sep="")),
        conditionalPanel(
          condition="input.entrymode=='box'",
          numericInput("quantiletb", "Enter X Axis Quantile (t) Value (positive values only):", 
                       value=2.00,
                       min = 0, 
                       max = 5.3,
                       step=.001))
      )
      
      
    ),
    br(), 
    downloadButton('downloadPDF', label = "Download plot as .pdf file"),
    br(),
    downloadButton('downloadPNG', label = "Download plot as .png file")
    ),
# Show a plot of the generated distribution
  mainPanel(
    tags$head(
      tags$style(HTML("
                      .shiny-output-error-validation {
                      font-size: 20px; color: blue;
                      }
                      "))
      ),
    tabsetPanel(
      tabPanel("Plot",
               plotOutput("distPlot"),
               br(),br(),
               conditionalPanel(
                 condition="input.df > 7 &input.control=='quant'",
               HTML("Extreme quantile values may not display if they are out of the range of this plot's X axis.")
               ),
               conditionalPanel(
                 condition="input.control=='prob'",
                 HTML("Critical values may not display if the tail probability is less than .00001, but they will still be printed at the top of the graph.")
               )
               ),
      tabPanel("About", 
               includeMarkdown('about.md'))
    )) # end tabPanel and mainpanel
))

