# Define UI for application that plots binomial distributions
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Binomial Distribution Visualization"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("pr", 
                "Probability of a Success:", 
                min = .0,
                max = 1.0, 
                value = .50,
                step=.01,sep=""),
    br(),
    numericInput("size",
                 "Number of trials (n):",
                 min=1,
                 max=500,
                 value=4,
                 step=1,
                 width='65%'),
    br(),
    br(),

    checkboxInput(inputId="regions",
                  label = strong("Find probabilities for regions using Cut Points"),
                  value=FALSE),
    conditionalPanel(condition="input.regions",
                     HTML("Enter whole number values in one or both of the following boxes 
                          to find probabilities of regions. 
                          THE MAXIMUM PERMISSIBLE VALUE WOULD BE n AND THE MINIMUM IS ZERO.  
                          Only enter whole numbers (Successes)")
                     ,
                     wellPanel(
                     numericInput("cut1",
                                  "First Cut Off (typically higher)",
                                  min=0,
                                  max="input.size",
                                  value=NA,
                                  step=1,
                                  width="90%"),
                     numericInput("cut2",
                                  "Second Cut Off (typically lower, if used at all)",
                                  min=0,
                                  max="input.size",
                                  value=NA,
                                  step=1,
                                  width="90%"),
                     HTML("Note that for regions with extremely large or small probabilities those probabilities may round 
                           to 1 or zero")
                     )
    ),
   conditionalPanel(condition = "input.size >10 && !input.regions",
                     HTML("The range of x-axis values on this plot may adjusted to less than the full distribution range when n > 10.  Control that with the checkbox below. </p>"),
                     checkboxInput(inputId = "scale2",
                                   label = strong("Show full scale of possible values (Successes)"),
                                   value = FALSE)),
    checkboxInput(inputId="probs",
                  label = strong("Create table of all binomial probabilities"),
                  value=FALSE)
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
               br(),
               downloadButton('downloadPlotpdf', label = "Download plot as PDF"),
               br(),
               downloadButton('downloadPlotpng', label = "Download plot as png"),
               br(),br(),
               conditionalPanel(condition="input.probs",
                                tableOutput("tab1"))),
            tabPanel("About", 
              includeMarkdown('about.md'))
    ))
))
