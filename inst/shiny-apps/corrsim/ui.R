# UI for app
shinyUI(pageWithSidebar(
  # title
  headerPanel("Bivariate Correlation Simulation and Bivariate Scatter Plotting"),
  #input
  sidebarPanel
  (    
    radioButtons("plottype", "Choose Type of Plot",
                 c("Simulated Bivariate Scatterplot" = "sim",
                   "Upload Data for a Bivariate Scatterplot" = "data")),
    conditionalPanel(condition = "input.plottype == 'sim'",                
                     wellPanel(
    sliderInput(inputId = "rho1",
                label = "Population Correlation (Rho)",
                min = -1, max = 1, value = .8, step = 0.01,sep=""),
    numericInput("mux", "IV (X) Mean",
                 min=-1000,
                 max=1000,
                 step=1,
                 value=20),
    numericInput("muy", "DV (Y) Mean",
                 min=-1000,
                 max=1000,
                 step=1,
                 value=40),
    numericInput("sdx", "IV (X) Std. Dev.",
                 min=.01,
                 max=100,
                 step=1,
                 value=2),
    numericInput("sdy", "DV (Y) Std. Dev.",
                 min=.01,
                 max=100,
                 step=.011,
                 value=3),
   numericInput("n", "Sample Size",
                min=2,
                max=1000,
                step=1,
                value=50),
    checkboxInput(inputId = "regline",
                  label = strong("Show Regression Line"),
                  value = FALSE),
    checkboxInput(inputId = "sdlinepos",
                 label = strong("Show SD Line (positive)"),
                 value = FALSE),
    checkboxInput(inputId = "sdlineneg",
                 label = strong("Show SD Line (negative)"),
                 value = FALSE),
    checkboxInput(inputId = "rugs",
                  label = strong("Show Rug Plots for Y and Yhat"),
                  value = FALSE),
   br(),
   actionButton("draw", "Resample with same parameters")
#finish sim wellpanel and conditionalpanel
                     )),
  conditionalPanel(condition = "input.plottype == 'data'",
#   add data input stuff here
   wellPanel(
     helpText("Note: read instructions in the Uploading Data Tab first!!!"),
     fileInput("upload", "Upload a CSV file:"), 
      #tags$hr(),
      checkboxInput('header', 'csv file has a Header row', TRUE),
#      radioButtons('sep', 'Separator',
#                   c(Comma=',',
#                     Semicolon=';',
#                     Tab='\t'),
#                   'Comma'),
#      radioButtons('quote', 'Quote',
#                   c(None='',
#                     'Double Quote'='"',
#                     'Single Quote'="'")),
#                   'Double Quote')
    wellPanel(
     uiOutput("varsets"))
#              
      ),
    checkboxInput(inputId = "regline2",
             label = strong("Show Regression Line"),
             value = FALSE),
   checkboxInput(inputId = "loess2",
             label = strong("Show Loess Fit"),
             value = FALSE),
   HTML("&nbsp; &nbsp; &nbsp; 
        <a href='http://en.wikipedia.org/wiki/Local_regression', target='_blank'>
        What is a Loess Fit?</a>"),
   checkboxInput(inputId = "rugs2",
             label = strong("Show Rug Plots for Y and Yhat"),
             value = FALSE)                   
                   )
    #finish sidebarpanel
    ),

mainPanel(
    tabsetPanel(
      tabPanel("Plot",
        uiOutput("plot")),
      tabPanel("Uploading Data", 
               includeMarkdown('uploaddata.md')),
      tabPanel("About", 
               includeMarkdown('about.md'))
    ))
  ))  
