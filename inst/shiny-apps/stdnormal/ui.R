#library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Tail probability of Standard Normal Distribution"),
  
  # Sidebar with controls
  sidebarPanel(
    selectInput(
       "control", "Choose Type of Control:",
       c("Adjust Probability" = "prob", "Adjust X-axis quantile (Z) value"="quant")
       ),
    selectInput("tail","Which Tail of Distribution?",
                list("one-tailed upper" = "upper", 
                     "one-tailed lower" = "lower", 
                     "two-tailed" = "two")),    
    radioButtons("entrymode", "Choose slider or direct text/number entry:",
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
                value = .05000,
                step=.001,sep="")),
     conditionalPanel(
       condition="input.tail=='lower'",
       sliderInput("prl", 
                 "Tail Probability:", 
                 min = .00001,
                 max = .50, 
                 value = .05000,
                 step=.001,sep="")),
     conditionalPanel(
       condition="input.tail=='two'",       
       sliderInput("prt", 
                 "Tail Probability:", 
                 min = .00001,
                 max = 1.0, 
                 value = .0500,
                 step=.001,sep=""))
     ),
     conditionalPanel(
       condition="input.entrymode=='box'",
       conditionalPanel(
         condition="input.tail=='upper'",
         numericInput("pvalu", "Enter Probability:", 
                      value=.0500,
                      min = 0, 
                      max = 1.00,
                      step=.001)),
       conditionalPanel(
         condition="input.tail=='lower'",
         numericInput("pvall", "Enter Probability:", 
                      value=.0500,
                      min = 0, 
                      max = 1.00,
                      step=.001)),
       conditionalPanel(
         condition="input.tail=='two'",       
         numericInput("pvalt", "Enter Probability:", 
                      value=.0500,
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
                          "X-Axis Quantile (Z) Value", 
                          min = 0,
                          max = 4.3, 
                          value = 1.65000,
                          step=.001,sep="")),
              conditionalPanel(
                condition="input.entrymode=='box'",
                numericInput("quantileub", "Enter X Axis Quantile (Z) Value:", 
                             value=1.65,
                             min = -4.3, 
                             max = 4.3,
                             step=.001))
              ),
      
      conditionalPanel(
        condition="input.tail=='lower'",
        conditionalPanel(
          condition="input.entrymode=='slider'",
          sliderInput("quantilel", 
                      "X-Axis Quantile (Z) Value", 
                      min = -4.3,
                      max = 0, 
                      value = -1.65000,
                      step=.001,sep="")),
        conditionalPanel(
          condition="input.entrymode=='box'",
          numericInput("quantilelb", "Enter X Axis Quantile (Z) Value:", 
                       value=-1.65,
                       min = -4.3, 
                       max = 4.3,
                       step=.001))
      ),
      
      conditionalPanel(
        condition="input.tail=='two'",
        conditionalPanel(
          condition="input.entrymode=='slider'",
          sliderInput("quantilet", 
                      "X-Axis Quantile (Z) Value", 
                      min = 0,
                      max = 4.3, 
                      value = 1.96000,
                      step=.001,sep="")),
        conditionalPanel(
          condition="input.entrymode=='box'",
          numericInput("quantiletb", "Enter X Axis Quantile (Z) Value (positive values only):", 
                       value=1.96000,
                       min = 0, 
                       max = 4.3,
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
               plotOutput("distPlot")),
      tabPanel("About", 
               includeMarkdown('about.md'))
    ))
))

