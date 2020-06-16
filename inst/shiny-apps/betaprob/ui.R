library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Visualize Type I/II errors: One-sample Test of Means (Z test)"),
  

  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    
    #Choose Tail of Test
    radioButtons("tail", "Choose Tail of the Test",
                 c("One Tail, Upper Tail" = "upper",
                   "One Tail, Lower Tail" = "lower",
                   "Two Tail" = "two")),
    tags$strong("Choose Plot to Display"),
    checkboxInput("plotshownull","Show Null Hypothesis Sampling Distribution", TRUE),
    checkboxInput("plotshowalt","Show Alternative Hypothesis Sampling Distribution", TRUE),
    #Alpha Control
    radioButtons("control", "Choose alpha control via slider or menu",
                 c("Choose among several fixed alpha levels"= "menu",
                   "Use a slider for alpha choice"="slidr")),
    wellPanel(
      conditionalPanel(
        condition="input.control =='slidr'",
        sliderInput("alpha", "Alpha, Type I Error rate", 
                    min = .0005, max = .15, value = .05, step= 0.0005,sep="")
      ),
      conditionalPanel(
        condition="input.control=='menu'",
        radioButtons("alpha2", "Alpha, the Type I Error rate",
                     c(".15"=.15,
                       ".10"=.10,
                       ".05"=.05,
                       ".025"=.025,
                       ".020"=.020,
                       ".01"=.01,
                       ".005"=.005,
                       ".001"=.001,
                       ".0005"=.0005),selected=.05,inline=T)
      )
    ),    
    
    
    
    
    
    # Distribution Control
    conditionalPanel(
      condition="input.tail =='upper'",
    numericInput("mean1", "Null Hypothesis Mean", 
               value = 100),
	  numericInput("mean2", "Alternative Hypothesis Mean", 
               value = 105),
  	numericInput("sem", "Standard Error of the Mean", 
               value = 5)
    ),
    conditionalPanel(
      condition="input.tail =='lower'",
      numericInput("mean1b", "Null Hypothesis Mean", 
                   value = 100),
      numericInput("mean2b", "Alternative Hypothesis Mean", 
                   value = 95),
      numericInput("semb", "Standard Error of the Mean", 
                   value = 5)
    ),
conditionalPanel(
  condition="input.tail =='two'",
  numericInput("mean1c", "Null Hypothesis Mean", 
               value = 100),
  numericInput("mean2c", "Alternative Hypothesis Mean", 
               value = 105),
  numericInput("semc", "Standard Error of the Mean", 
               value = 5)
)

    
  ),

  
  # Show a table summarizing the values entered
  #  mainPanel(
  #  plotOutput(outputId = "main_plot", height = "500px",width="600px"),
  #  HTML("This application is still under development, but should work well 
  #       for one-tailed test situations. 
  #       <br><br>
  #       Later revisions will include two-tailed alternatives.")  )
  
  mainPanel(
    tabsetPanel(
      tabPanel("Plot",
               plotOutput(outputId = "main_plot", height = "500px",width="600px"),
      br(),
      downloadButton('downloadPlotpdf', label = "Download plot as PDF"),
      br(),
      downloadButton('downloadPlotpng', label = "Download plot as png")
      ),
      tabPanel("About", 
               includeMarkdown('about.md'))
    ))
))
