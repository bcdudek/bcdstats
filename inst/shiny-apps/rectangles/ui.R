# -------------------------------------------------------
#   App Title: Visualizing Correlation and Covariance
#      Author: Bruce Dudek
# -------------------------------------------------------

shinyUI(navbarPage(theme="bootstrap_flatly.css", "Correlation and Covariance Visualiation App with Shiny", 
#                   tags$head(
#                     tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap_flatly.css")
#                   ),      
#----------------------------------------------------------------------------------------------------------------------------------------

tabPanel("Intro",
           withMathJax(),
           p("We use the Correlation coefficient to quantify the strength and direction of
           a relationship between two variables.   \\(e.g.\\), think about height and weight, or hours of sleep and irritability.",br(), br(),
             "\\(\\bullet\\) The Pearson product-moment correlation coefficient is scale free and it ranges between -1 and 1.",br(),

            "\\(\\bullet\\) It is typically denoted by \\(r\\), for sample data or by \\(\\rho\\) (the greek symbol Rho), to indicate the population value.",br(),
             
             "\\(\\bullet\\) You have probably examined XY scatterplots to visualize this type of bivariate relationship, and have begun to evaluate the 
             2 dimensional attributes of the scattercloud to gain a sense of direction and strength of the relationship.",br(),
            br(),
            "\\(\\bullet\\) Often, introductory textbooks show a figure like the following which depicts a series of XY scatterplots reflecting 
            correlation patterns of differing size and sign.  This one is the Wikipedia illustration.",br(),
br()
),
div(img(src="Correlation_examples3.png")),
br(),

p(
"\\(\\bullet\\) A correlation of -1 means that the X and Y variables have a perfect negative relationship and the data points fit
           a straight line with a negative slope.",br(),
"\\(\\bullet\\) Similarly, a correlation of  +1  means that X and Y have a perfect positive relationship and fall on a line with positive slope.",br(),
"\\(\\bullet\\) If X and Y variables have no relationship, the correlation is 0, and the array of points in the scatterplot have no pattern/orientation.",
br(),
br()
),
div(
h4(em("This shiny app permits interactive visual exploration via simulations that enable the user better to understand patterns of bivariate (XY) data and 
the strength and sign of the relationship.")), 
h4(em("Additional visual approaches facilitate the understanding of the computations used for the correlation coefficient
and the important central role of the Covariance statistic.")),  

h4(em("The user is advised to work through each tab, in order.")
)),
br(),
div("Shiny app by", 
    a(href="http://www.albany.edu/psychology/20869.php",target="_blank", 
      "Bruce Dudek"),align="left", style = "font-size: 9pt")
  ), #finish intro tabpanel

#----------------------------------------------------------------------------------------------------------------------------------------

navbarMenu("Visualize Relationships",
#----------------------------------------------------------------------------------------------------------------------------------------
           
    tabPanel("1.  Relationship Direction and Strength",
             p(h5("Simulation No. 1.  Visualize direction of relationship with Quadrants.")),
             p("\\(\\bullet\\) If we randomly sample Y and X variables from a bivariate normal 
                population/distribution,we can draw an XY scatterplot of 
                the N data points.",br(),
               "\\(\\bullet\\) XY Relationships can be explored here by changing parameters and by
               resampling.",br(),
               "\\(\\bullet\\) Note that even though you may fix \\(\\rho\\) at one value, the 
                \\({r_{xy}}\\) values will change with resampling.  This reflects the sampling 'noise' of
               the statistic.  Try to see how increasing sample size influences this variation in r from sample to sample",br()
               ),
             br(), 
               
             
             #div(h5("XY relationships and scatterplot patterns can be explored here:")),
    sidebarPanel(

      radioButtons("plottype1", "Choose Plot Type:",
                   c("Base Scatterplot" = "base",
                     "Show X and Y Means and Quadrants"= "quadrants")),

            conditionalPanel(condition="input.plottype1=='base'", 
               sliderInput(inputId = "rho1",
                           label = "Population Correlation (\\(\\rho\\))",
                           min = -1, max = 1, value = .8,
                           step=.01, animate=list(loop=TRUE, interval=400),sep=""),
               div(class="row",
                   div(class="col-xs-6",
                       numericInput("mux1", "IV (X) Mean (\\(\\mu_X\\))",
                                     min=-1000,
                                     max=1000,
                                     step=1,
                                     value=20)),
                   div(class="col-xs-6",
                           numericInput("muy1", "DV (Y) Mean (\\(\\mu_Y\\))",
                                        min=-1000,
                                        max=1000,
                                        step=1,
                                        value=40)                         
                       )
                       ),              
               div(class="row",
                   div(class="col-xs-6",
                       numericInput("sdx1", "IV (X) Std. Dev. (\\(\\sigma_X\\))",
                                    min=.01,
                                    max=100,
                                    step=1,
                                    value=2)),
                   div(class="col-xs-6",
                       numericInput("sdy1", "DV (Y) Std. Dev. (\\(\\sigma_Y\\))",
                                    min=.01,
                                    max=100,
                                    step=.011,
                                    value=3)                         
                   )
               ),  
               numericInput("n1", "Sample Size",
                            min=2,
                            max=1000,
                            step=1,
                            value=50),
               checkboxInput("regline1", "Show best fit line, the regression line"),
               actionButton("draw", "Resample with same parameters")
      ),# finishes conditionalpanel called base
      conditionalPanel(condition="input.plottype1=='quadrants'",
                       #p(h4("Simulation No. 1. Visualizing Data in Quadrants")),br(),
                       sliderInput(inputId = "rho2",
                                   label = "Population Correlation (\\(\\rho\\))",
                                   min = -1, max = 1, value = .8,
                                   step=.01, animate=list(loop=TRUE, interval=400),sep=""),
                       numericInput("n2", "Sample Size",
                                    min=2,
                                    max=1000,
                                    step=1,
                                    value=50),

                       checkboxInput("regline2", "Show best fit line, the regression line"),
                       checkboxInput("quadtable", "show a table of counts of data points in each quadrant"),
                       actionButton("draw2", "Resample with same parameters"),
                       br(),
                       p("\\(\\bullet\\) Notice that the difference in number of points found in the Top Right/Lower 
                         Left quadrants from the Lower Left/Top Right quadrants is a helpful way of 
                         seeing why the sign of the correlation is either positive or negative.",  
                         em("Where do points cluster for positive vs negative corrlations?"),br(),
                         "\\(\\bullet\\)  Also notice that the location of each data point in a quadrant is determined by 
                          how far above or below the respective X and Y means that the data point resides.
                          this perspective leads
                         the N data points.",br(),
                       "\\(\\bullet\\)  These perspectives lead to the core way of understanding the computation 
                       of the correlation coefficient and the covariance as seen with simulations in the next three sections of this app.")
      )# finishes conditionalpanel called quadrants      
      

      
   ), #finish sidebar panel for intro/base plus quadrants

             mainPanel(
               uiOutput("tab3boutput"))
                  #plotOutput("plot1")
          #)

   
    ),# finish tabpanel base scatterplot

#----------------------------------------------------------------------------------------------------------------------------------------

    tabPanel("2.  Visualize the computation",
             withMathJax(),
             p(h5("Simulation No. 2.  Visualizing the Cross-Product computation.")),br(),
             p("\\(\\bullet\\) Rho (\\(\\rho_{xy}\\)), the population correlation is defined as 
              \\(\\frac{{{\\sigma _{xy}}}}{{{\\sigma _x}{\\sigma _y}}}\\).  The numerator is the Covariance of
              X and Y, and the denominator is the product of the two standard deviations.",
               br(),
               "\\(\\bullet\\) With sample data \\({r_{xy}}\\) is computed as 
                  \\(\\frac{{Co{v_{xy}}}}{{{s_x}{s_y}}}\\).  The covariance term in the numerator is the central
                  statistic in determining the relationship between the X and Y variables.",br(),
                " \\(\\bullet\\) The \\(Co{v_{xy}}\\) is calculated with sample data as  
                  \\(\\frac{{\\sum\\limits_{i = 1}^n {({X_i} - \\bar X)({Y_i} - \\bar Y)} }}{{n - 1}}\\).  
                  The numerator of the covariance statistic is call the Sum of Cross-Products (SP).",br(),
               "\\(\\bullet\\) Each pair of X and Y values contribute this deviation product and each is called a Cross-Product.",br(),
               "\\(\\bullet\\) The simulation here enables visualization of one of these cross-products and enables the viewer to 
               relate it to the quadrant-location perspective seen in simulation 3."
               
               ),
             br(),
             sidebarPanel(
               HTML("This simulation shows the data point that has the largest cross product, 
                    and its deviation components."),br(),
               br(),
               HTML("Start with Base ScatterPlot and then add:"),
               div(class="row",
                   div(class="col-xs-6",
                       checkboxInput("xbar4", label=("\\(\\bar X\\) Line"), value=FALSE)),
                   div(class="col-xs-6",
                       checkboxInput("ybar4", label=("\\(\\bar Y\\) Line"), value=FALSE))
               ), 
               div(class="row",
                   div(class="col-xs-6",
                       checkboxInput("xarrow4", label=("X Deviation Arrow"), value=FALSE)), 
                   div(class="col-xs-6",
                       checkboxInput("yarrow4", label=("Y Deviation Arrow"), value=FALSE))
               ),  
               checkboxInput("cpvalue", label=("Show the deviation values and CP for this point"), value=FALSE), 
               br(),
               sliderInput(inputId = "rho4",
                           label = "Population Correlation (\\(\\rho\\))",
                           min = -1, max = 1, value = .8, step = 0.01,sep=""),
              numericInput("n4", "Sample Size",
                           min=2,
                           max=1000,
                           step=1,
                           value=20),
              br(),
              actionButton("draw4", "Resample with same parameters"),
              br()
              
            ), #finish sidebar panel for intro/base
             mainPanel(  
               #textOutput("ppm"),
               uiOutput("note1"),
               uiOutput("viscalc")),
               uiOutput("tablevalues4")

    ), #finish tabpanel
tabPanel("3.  Cross Product as Rectangle",
         withMathJax(),
         p(h5("Simulation No. 3.  Visualizing the Cross-Product as a Rectangle.")),br(),
         p("\\(\\bullet\\) We saw above, that the covariance statistc is central to the concept of a correlation.  
           It is computed with the following expression: 
           \\(\\frac{{\\sum\\limits_{i = 1}^n {({X_i} - \\bar X)({Y_i} - \\bar Y)} }}{{n - 1}}\\). ",
           br(),
           "\\(\\bullet\\) The numerator of the covariance was defined as the Sum of Cross Products, 
           \\(\\sum\\limits_{i = 1}^n {({X_i} - \\bar X)({Y_i} - \\bar Y)}\\), often abbreviated as SP.",br(),
           
           " \\(\\bullet\\) The SP is a summation of a multiplication operation done on each case and we have 
           defined this multiplication as a Cross Product: \\(({X_i} - \\bar X)({Y_i} - \\bar Y)\\).",br(),
           "\\(\\bullet\\)We examined these deviations in Simulation 2 and alluded to the area that they circumscribe as a rectangle",br(),
           "\\(\\bullet\\) The simulation here enables explicit visualization of one of these cross-products 
           as a rectangle.",br(),
           em("\\(\\bullet\\)  Since the area of a rectangle is the product of two adjacent sides, 
              the value of a Cross Product can be seen as the area of a rectangle.")
           ),
         br(),  
         sidebarPanel(           

           p(h5("Start with the base scatterplot, and then add the rectangle")),br(),
           checkboxInput("showrect1", "Show the Cross Product for the largest Y value"),
           checkboxInput("values5", "Show the component values for the case with the largest Y value"),
           sliderInput(inputId = "rho5",
                       label = "Population Correlation (\\(\\rho\\))",
                       min = -1, max = 1, value = .8, step = 0.01,sep=""),
           div(class="row",
               div(class="col-xs-6",
                   numericInput("mux5", "IV (X) Mean (\\(\\mu_X\\))",
                                min=-1000,
                                max=1000,
                                step=1,
                                value=20)),
               div(class="col-xs-6",
                   numericInput("muy5", "DV (Y) Mean (\\(\\mu_Y\\))",
                                min=-1000,
                                max=1000,
                                step=1,
                                value=40)                         
               )
           ),              
           div(class="row",
               div(class="col-xs-6",
                   numericInput("sdx5", "IV (X) Std. Dev. (\\(\\sigma_X\\))",
                                min=.01,
                                max=100,
                                step=1,
                                value=2)),
               div(class="col-xs-6",
                   numericInput("sdy5", "DV (Y) Std. Dev. (\\(\\sigma_Y\\))",
                                min=.01,
                                max=100,
                                step=.011,
                                value=3)                         
               )
           ),  
           numericInput("n5", "Sample Size",
                        min=2,
                        max=1000,
                        step=1,
                        value=50),
           checkboxInput("regline5", "Show best fit line, the regression line"),
           actionButton("draw5", "Resample with same parameters")
         ),
         mainPanel(  
           uiOutput("rectangle1"),
           uiOutput("tablevalues5"))
         
         ), #finish tabpanel

#----------------------------------------------------------------------------------------------------------------------------------------

tabPanel("4.  Visualizing Covariance Comptuation",
         withMathJax(),
         p("\\(\\bullet\\) Simulation No. 4.  Visualize All Cross-Products as Rectangles.",br(),
           "\\(\\bullet\\) In this simulation positive and negative Cross Products can be visualized. ",
           br(),
           "\\(\\bullet\\) Since the Sum of Cross Products (\\(\\sum\\limits_{i = 1}^n {({X_i} - \\bar X)({Y_i} - \\bar Y)}\\)) 
           is the sum of all of these areas, 
           it can be visualized as the aggregate of the positively signed rectangle areas and 
           the negatively signed rectangle areas.  Consistent with the sign of the correlation coefficient, 
           this sum can be either negative or positive.",br(),
           " \\(\\bullet\\) Manipulation of the scaling aspects by changing scale (SD's) can change the SP dramatically, even when the
           correlation is held constant.  ",br()
         ),
         br(),  
         sidebarPanel(           
           radioButtons("scaling","Choose Variable Scaling",
                        c("Use Simulated Variables that have been standardized" = "scalefree",
                          "Use Variables that have not been standardized" = "scaled")),
           p(h5("Start with the base scatterplot, and then add the rectangles")),
           radioButtons("plottype6","Choose Type of Plot",
                        c("Base Scatterplot" = "base",
                          "Show Rectangles for Positive Cross Products (Blue)" = "positive6",
                          "Show Rectangles for Negative Cross Products (Red)" = "negative6",
                          "Show Rectangles for ALL Cross Products" = "both6",
                          "Frequency Histogram of all Cross Product Values" = "hist",
                          "Packed Rectangles" = "packed")),
           radioButtons("showdata6", "Show tables of values",
                        c("None"= "none",
                          "Summary Statistics"= "summary",
                          "Data and Components" = "data")
           ),
           sliderInput(inputId = "rho6",
                       label = "Population Correlation (\\(\\rho\\))",
                       min = -1, max = 1, value = .8, step = 0.01,sep=""),
           checkboxInput("regline6", "Show best fit line, the regression line"),
           conditionalPanel(condition="input.scaling == 'scaled'",
           div(class="row",
               div(class="col-xs-6",
                   numericInput("mux6", "IV (X) Mean (\\(\\mu_X\\))",
                                min=-1000,
                                max=1000,
                                step=1,
                                value=20)),
               div(class="col-xs-6",
                   numericInput("muy6", "DV (Y) Mean (\\(\\mu_Y\\))",
                                min=-1000,
                                max=1000,
                                step=1,
                                value=40)                         
               )
           ),              
           div(class="row",
               div(class="col-xs-6",
                   numericInput("sdx6", "IV (X) Std. Dev. (\\(\\sigma_X\\))",
                                min=.01,
                                max=100,
                                step=1,
                                value=2)),
               div(class="col-xs-6",
                   numericInput("sdy6", "DV (Y) Std. Dev. (\\(\\sigma_Y\\))",
                                min=.01,
                                max=100,
                                step=.011,
                                value=3)                         
               )
           ),  
           numericInput("n6", "Sample Size",
                        min=2,
                        max=1000,
                        step=1,
                        value=50)
           ), #finish conditionalPanel
           actionButton("draw6", "Resample with same parameters")
         ),
         mainPanel(  
           uiOutput("rectangles"),
           uiOutput("tablevalues6")),
         uiOutput("tablevalues6b")           
         ) #finish tabpanel for covariance visualization

), # finish navbarMenu for visualizations

#----------------------------------------------------------------------------------------------------------------------------------------

#navbarMenu("Visualize Covariance",
#----------------------------------------------------------------------------------------------------------------------------------------
           

# ),# finish navbarMenu2 
#----------------------------------------------------------------------------------------------------------------------------------------

# tabPanel("Bivariate Normal",
#          p(
#            "\\(\\bullet\\) When measurements on X and Y variables are obtained, the scores obtained comprise a sample",br(),
#            "\\(\\bullet\\) We assume that the sample is randomly drawn from a population, often of infinite size",br(),
#            "\\(\\bullet\\) With the methods of correlation and regression, we use linear mathematical models, and we usually assume that the  population distribution is bivariate normal.",br(),
#            "\\(\\bullet\\) This app section permits simulation of bivariate normal distributions, where X and Y dimensions exist, as in a scatterplot, but the third dimension is density (i.e., relative frequency of occurence.",br(),
#            br(),
#            br()
#          ),
#          sidebarPanel(
#            p(h4("Simulation No. 1. Visualizing the Bivariate Normal")),br(),
#            sliderInput("rho", "Adjust \\(\\rho\\) (rho), the distribution correlation", 
#                        -.80, .80, -.80, .05, animate=list(loop=TRUE, interval=500),sep=""),
#            sliderInput("angle", "Rotate the graph angle in degrees",  
#                        0, 360, 112.5, 22.5, animate=list(loop=TRUE),sep=""),br(),
#            br(),
#            div(h5("This simulation uses the approach provided in the HH package in R"))
#          ),
#          
#          mainPanel(
#            plotOutput("bivariatedensityPlot")
#          )
#          
# ), #finish tabpanel for biv normal


#----------------------------------------------------------------------------------------------------------------------------------------

# tabpanel 5
tabPanel("About",
         includeMarkdown('about.md')
)
#----------------------------------------------------------------------------------------------------------------------------------------


 ))                 
