library(shiny)

# Define UI Scatterplot Graph View
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Orthogonal Polynomial Trend Analysis"),
  
  sidebarPanel(
    helpText("Adjust parameter values with the sliders or animate with the play buttons"),
    sliderInput("linwt", "Linear Parameter", min=-30, max=30, value=20,step=.5,
                animate=animationOptions(interval = 100, loop = T),sep=""), 
    sliderInput("quadwt", "Quadratic Parameter", min=-20, max=20, value=-8,step=.5,
                animate=animationOptions(interval = 100, loop = T),sep=""), 
    sliderInput("cubicwt", "Cubic Parameter", min=-20, max=20, value=0,step=.5,
                animate=animationOptions(interval = 100, loop = T),sep=""), 
    sliderInput("quartwt", "Quartic Parameter", min=-20, max=20, value=0,step=.2,
                animate=animationOptions(interval = 100, loop = T),sep=""), 
    #submitButton("Update View"),
    br(),
    helpText("Note: The Grand Mean of this data system is set to 175"),
    br(),
    h5("Orthogonal Polynomial Trend Coefficients Used:"),
    tableOutput("tab2")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Plot/Table",
               h3("Assume a 5-Level IV and ANOVA approach"),
               h5("Examine changes in response function shape as a result of trend component variation"),
               plotOutput("plot"),
               tableOutput("tab1")),
      tabPanel("About", 
               includeMarkdown('about.md'))
    ))
))
