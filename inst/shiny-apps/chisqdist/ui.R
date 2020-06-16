library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Upper Tail probability of Chi Square Distribution"),
  
  # Sidebar with controls
  sidebarPanel(
    selectInput(
      "control", "Choose Type of Control:",
      c("Adjust Probability" = "prob", "Adjust X-axis quantile (Chi square) value"="quant")
    ),
    numericInput("df",
                 "Degrees of Freedom",
                 min=1,
                 max=1000,
                 value=1),
    radioButtons("entrymode", "Use slider or direct text entry:",
                 c("Slider" = "slider",
                   "Number Entry Box (recommended for quantile control)" = "box")
                 ),   
    conditionalPanel(
      condition="input.control=='prob'",
      conditionalPanel(
        condition="input.entrymode=='slider'",
          sliderInput("pr", 
                      "Upper Tail Probability:", 
                      min = .00001,
                      max = .50, 
                      value = .050,
                      step=.00001,round=F,sep="")),
      conditionalPanel(
        condition="input.entrymode=='box'",
          numericInput("pvalu", "Enter Probability:", 
                       value=.05,
                       min = .00001, 
                       max = 1.00,
                       step=.00001))),
    conditionalPanel(
      condition="input.control=='quant'",
      conditionalPanel(
        condition="input.entrymode=='slider'",
        sliderInput("quantile", 
                    "X-Axis Quantile (Chi square) Value", 
                    min = 001,
                    max = 50, 
                    value = 2,
                    step=.01,sep="")),
      conditionalPanel(
        condition="input.entrymode=='box'",
        numericInput("quantileb", "Enter X Axis Quantile (Chi square) Value:", 
                     value=2,
                     min = .001, 
                     step=.001))
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

