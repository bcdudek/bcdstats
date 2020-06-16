library(shiny)
library(rgl)
library(plot3D)
library(plot3Drgl)
# BCD COMMENT: THis app 

shinyUI(fluidPage(
  theme="superhero.bcd2.css",
  titlePanel(HTML("Linear Models with Two IVs:<br>
             <h4>Additive and Interaction Outcomes,  Moderation and Simple Slopes</h4>")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("ivtypes",
                  label= "Choose Type of Independent Variables",
                  choices = list("Two Categorical IV's" = "twocat",
                                 "One categorical IV and one Numeric IV" =  "onecat",
                                 "Two Numeric IV's" = "zerocat")),
      
      #################### here, control panels for Two categorical IV illustration
      conditionalPanel(
        condition = "input.ivtypes =='twocat'",
        radioButtons(
          "twocatplot",
          label = "Type of DataSet",
          choices = list(
            "2x2 ANOVA, No Interaction" = 1,
            "2x2 ANOVA, Interaction" = 2
          ),
          selected = 1
        ),
        # finish twocatplot radiobuttons
        
        conditionalPanel(
          condition = "input.ivtypes == 'twocat' & input.twocatplot == 1",
          radioButtons(
            "twocatadditive",
            label = "Choose type of Graph",
            choices = list(
              "Show Bar Graph of Means" = 1,
              "Add Simple Main Effects of Factor A to Bar Graph" = 2,
              "Add Simple Main Effects of Factor B to Bar Graph" = 3,
              "Show 3D Scatterplot and model surface" = 4
            ),
            selected = 1
          ),
          # finish radiobuttons for 2x2 additive bar vs 3d choices

          conditionalPanel(
            condition = "input.ivtypes == 'twocat'
            & input.twocatplot == 1
            & input.twocatadditive == 4",
            tags$hr(),
            radioButtons(
              "twocatadditive3d",
              label = "Choose 3D graph type",
              choices = list(
                "Show data only" = 1,
                "Show data plus regression surface" =
                  2,
                "Show only the cell means" =
                  3,
                "Show the cell means plus regression surface (plane)" =
                  4,
                "Add Simple Main Effects of Factor A" = 5
              )
            )
          )
      ),
      # finish condpaned for 2x2 additive and twocatplot is additive
      
      #now begin twocat graph where interaction present
      conditionalPanel(
        condition = "input.ivtypes == 'twocat' & input.twocatplot == 2",
        radioButtons(
          "twocatinteraction",
          label = "Choose type of Graph",
          choices = list(
            "Show Bar Graph of Means" = 1,
            "Add Simple Main Effects  of Factor A to Bar Graph" =2,
            "Add Simple Main Effects of Factor B to Bar Graph" =3,
            "Show 3D Scatterplot and model surface" = 4
          ),
          selected = 1
        ),
        # finish radiobuttons for 2x2 additive
        conditionalPanel(
          condition = "input.ivtypes == 'twocat'
          & input.twocatplot == 2
          & input.twocatinteraction == 4",
          tags$hr(),
          radioButtons(
            "twocatinteraction3d",
            label = "Choose 3D graph type",
            choices = list(
              "Show data only" = 1,
              "Show data plus additive model surface, a plane" = 2,
              "Show only means plus the additive surface" =  3,
              "Show means plus interaction surface (warped plane)" = 4,
              "Interaction surface plus simple main effects of Factor A" = 5
            )
          )
        )
    )# finish cond panel for 2x2 interaction and plot type is 3d
    #
    
    
    ), # finish conditioinalpanel on two categoricals choice
    
    ####################
    
    #################### here, control panels for one Cat, one numeric
    conditionalPanel(
      condition = "input.ivtypes =='onecat'",
      radioButtons(
        "onecatplot",
        label = "Type of 3D Scatter Plot",
        choices = list(
          "Traditional Bivariate Scatterplot" = 1,
          "3D Scatterplot, Data Only" = 2,
          "Add Additive Model Surface (plane)" = 3,
          "Add Interaction Model Surface (warped plane)" = 4
        ),
        selected = 1
      ),
      conditionalPanel(
        condition = "input.onecatplot == '4'",
        radioButtons(
          "onecatinteraction",
          label = "Choose Plot Attributes",
          choices = list(
            "Interaction Surface" = 1,
            "Add simple slopes to surface" = 2,
            "Display traditional Pequod Simple Slopes plot 
            instead of 3D scatterplot and surface." =3
          ),
          selected = 1
        ) #finish radiobuttons for onecatinteraction
        
      ) # finish conditionalpanel for warped surface plots in onecat
   
      
      
    ), #finish conditionalpanel for one categorical IVs
    ####################
    
    
    
    #################### here, control panels for Two numeric IV illustration
    conditionalPanel(
      condition = "input.ivtypes =='zerocat'",
      radioButtons(
        "twonumericplot",
        label = "Type of 3D Scatter Plot",
        choices = list(
          "Data Only" = 1,
          "Add Additive Model Surface (plane)" = 2,
          "Add Interaction Model Surface (warped plane)" = 3
        ),
        selected = 1
      ),
      conditionalPanel(
        condition = "input.twonumericplot == '3'",
        radioButtons(
          "twonumericinteraction",
          label = "Choose Plot Attributes",
          choices = list(
            "Interaction Surface only" = 1,
            "Add simple slopes to surface.  Ivs are now centered" =2,
            "Show traditional simple slopes plot from pequod package"=3
             ),
          selected = 1
        ), #finish radiobuttons
        conditionalPanel(
          condition="input.twonumericplot == '3' &
                     input.twonumericinteraction ==2",
            h6("Simple slopes are plotted at +1 and -1 SD of the moderator.  
             The four squares are the four points plotted 
                 in a traditional simple slopes line graph from 
                 software such as the Pequod package or
                 PROCESS macros." )
          
          
        )
       
      ) # finish conditionalpanel for warped surface plots
    ) #finish conditionalpanel for two numeric IVs
    ####################
                                   ),
    
    mainPanel(
      # BCD comment: This tag$head inserted by BCD to handle validation output and change tab colors
      tags$head(
        tags$style(HTML("
                        .shiny-output-error-validation {
                        font-size: 20px; color: skyblue;
                        }
                        ")),
        tags$style(HTML("
                        .tabbable > .nav > li > a                  {background-color: lightgray;  color:navy}
                        .tabbable > .nav > li > a[data-value='t1'] {background-color: red;   color:white}
                        .tabbable > .nav > li > a[data-value='t2'] {background-color: blue;  color:white}
                        .tabbable > .nav > li > a[data-value='t3'] {background-color: green; color:white}
                        .tabbable > .nav > li[class=active]    > a {background-color: lemonchiffon; color:navy}
                        "))
        ),
      tabsetPanel(
        tabPanel("Intro/Background",
                 HTML("This app provides visualizations that assist in the understanding of the concepts of 
interaction, moderator analysis, simple main effects, and simple slopes.  The approach focuses on the meaning of the 
regression surfaces produced by model fits, and the location of simple effects on those surfaces.<br><br>

All the examples presume a model with two independent variables in order to facilitate the development of a central concept in interaction analysis.
  That concept is that regardless of whether the independent variables are both categorical, one categorical and one numeric, or both numeric, that the meaning of 
interaction is the same thing.  In additional, the equivalence of the simple main effect concept in traditional ANOVA
 (two categorical IVs) is equivalent to the simple slopes concept in moderator analysis with one or two numeric IVs.<br><br>

Two IV regression models produce a surface fit that is a plane in models that do not include an interaction term.  These are called
additive models.  When the interaction term is included, the multiplicative model fits a warped plane.  The ability of R software to
produce 3D scatterplots has facilitated these visualizations.<br><br>

In addition, the user will have the ability to use the mouse to actively rotate the 3D scatterplot figures to provide an
enhanced visual understanding of the surfaces and the simple effect locations.<br><br>

The two-categorical-IVs example is a simulated data set that reduces to the traditional 2x2 ANOVA design.<br><br>

The one-categorical/one-numeric-IV example is the NHANES data set where systolic blood pressure is the DV and the IVs are smoking status and body weight.<br><br>

The two-numeric-IVs example is a data set where depression severity is the outcome variable and predictions from an index of anxiety severity and a measure of positive affect are the IVs.<br><br>

Details on the data sets are found in the ABOUT THE DATA SETS tab.<br><br>

This app is intended to be a supplement to a monograph on Interaction Analysis by B. Dudek, but can also suffice for standalone purposes.


                      
                      
                      ")),
        # finish first tabpanel
        tabPanel(
          "Plots/Summary",
          ########################
          # Each plot is displayed via a separate conditionalpanel  and the output is a different name
          ########################
          
          ########################
          ## plots for 2x2 AOV models
          ########################
          #2x2 additive bars
          conditionalPanel(
            condition = (
              "input.ivtypes == 'twocat' &
              input.twocatplot == 1 &
              input.twocatadditive == 1"
            ),
            plotOutput("barsa1", height = 400, width = 600)
            ),
          conditionalPanel(
            condition = (
              "input.ivtypes == 'twocat' &
              input.twocatplot == 1 &
              input.twocatadditive == 2"
            ),
            plotOutput("barsa2", height = 400, width = 600)
            ),
          conditionalPanel(
            condition = (
              "input.ivtypes == 'twocat' &
              input.twocatplot == 1 &
              input.twocatadditive == 3"
            ),
            plotOutput("barsa3", height = 400, width = 600)
            ),
          ########################
          # still 2x2 additive but now 3D
          conditionalPanel(
            condition = (
              "input.ivtypes == 'twocat' &
              input.twocatplot == 1 &
              input.twocatadditive == 4 &
              input.twocatadditive3d ==1"
            ),
            rglwidgetOutput("aovadd3D1", height = 400, width = 600)
            ),
          conditionalPanel(
            condition = (
              "input.ivtypes == 'twocat' &
              input.twocatplot == 1 &
              input.twocatadditive == 4 &
              input.twocatadditive3d ==2"
            ),
            rglwidgetOutput("aovadd3D2", height = 400, width = 600)
            ),
          conditionalPanel(
            condition = (
              "input.ivtypes == 'twocat' &
              input.twocatplot == 1 &
              input.twocatadditive == 4 &
              input.twocatadditive3d ==3"
            ),
            rglwidgetOutput("aovadd3D3", height = 400, width = 600)
            ),
          conditionalPanel(
            condition = (
              "input.ivtypes == 'twocat' &
              input.twocatplot == 1 &
              input.twocatadditive == 4 &
              input.twocatadditive3d ==4"
            ),
            rglwidgetOutput("aovadd3D4", height = 400, width = 600),
            HTML("The additive model surface, a plane, fits the cell means perfectly")
            ),
          conditionalPanel(
            condition = (
              "input.ivtypes == 'twocat' &
              input.twocatplot == 1 &
              input.twocatadditive == 4 &
              input.twocatadditive3d ==5"
            ),
            rglwidgetOutput("aovadd3D5", height = 400, width = 600)
            ),
          ###############
          # 2x2 interaction bars
          conditionalPanel(
            condition = (
              "input.ivtypes == 'twocat' &
              input.twocatplot == 2 &
              input.twocatinteraction == 1"
            ),
            plotOutput("barsi1", height = 400, width = 600)
            ),
          conditionalPanel(
            condition = (
              "input.ivtypes == 'twocat' &
              input.twocatplot == 2 &
              input.twocatinteraction == 2"
            ),
            plotOutput("barsi2", height = 400, width = 600)
            ),
          conditionalPanel(
            condition = (
              "input.ivtypes == 'twocat' &
              input.twocatplot == 2 &
              input.twocatinteraction == 3"
            ),
            plotOutput("barsi3", height = 400, width = 600)
            ),
          ########################
          # 2x2 interaction 3D
          conditionalPanel(
            condition = (
              "input.ivtypes == 'twocat' &
              input.twocatplot == 2 &
              input.twocatinteraction == 4 &
              input.twocatinteraction3d ==1"
            ),
            rglwidgetOutput("aovint3D1", height = 400, width = 600)
            ),
          conditionalPanel(
            condition = (
              "input.ivtypes == 'twocat' &
              input.twocatplot == 2 &
              input.twocatinteraction == 4 &
              input.twocatinteraction3d ==2"
            ),
            rglwidgetOutput("aovint3D2", height = 400, width = 600)
            ),
          conditionalPanel(
            condition = (
              "input.ivtypes == 'twocat' &
              input.twocatplot == 2 &
              input.twocatinteraction == 4 &
              input.twocatinteraction3d ==3"
            ),
            rglwidgetOutput("aovint3D3", height = 400, width = 600),
            HTML("The additive model surface, a plane, is an inadequate fit to the cell means")
            ),
          conditionalPanel(
            condition = (
              "input.ivtypes == 'twocat' &
              input.twocatplot == 2 &
              input.twocatinteraction == 4 &
              input.twocatinteraction3d ==4"
            ),
            rglwidgetOutput("aovint3D4", height = 400, width = 600),
            HTML("The Interaction surface, a warped plane, fits the cell means perfectly")
            ),
          conditionalPanel(
            condition = (
              "input.ivtypes == 'twocat' &
              input.twocatplot == 2 &
              input.twocatinteraction == 4 &
              input.twocatinteraction3d ==5"
            ),
            rglwidgetOutput("aovint3D5", height = 400, width = 600)
            ),
          #############################
          # finished with 2x2 plots
   
          ##############################
          #onecat plots
          
          #onecat biv scatterplot
          conditionalPanel(
            condition = ("input.ivtypes == 'onecat' &
                         input.onecatplot == 1"),
            plotOutput("onecatbivariate", width = 600)
            ),       
          
          #onecat 3D data only
          conditionalPanel(
            condition = ("input.ivtypes == 'onecat' &
                         input.onecatplot == 2"),
            rglwidgetOutput("onecat3d1", width = 600)
            ),       

          #onecat 3D data plus additive plane
          conditionalPanel(
            condition = ("input.ivtypes == 'onecat' &
                         input.onecatplot == 3"),
            rglwidgetOutput("onecat3d2", width = 600),
            HTML("This additive model is underspecified since there is an interaction between 
                 smoker status and body weight.")
          ),             
          
          #onecat 3D data plus interaction surface
          conditionalPanel(
            condition = ("input.ivtypes == 'onecat' &
                         input.onecatplot == 4 &
                         input.onecatinteraction == 1"),
            rglwidgetOutput("onecat3d3", width = 600),
            HTML("The presence of the interaction suggests that the predictability of 
                 blood pressure from body weight is moderated by smoking status.")
          ),       
             
          #onecat 3D data plus interaction surface plus simple slopes
          conditionalPanel(
            condition = ("input.ivtypes == 'onecat' &
                         input.onecatplot == 4 &
                         input.onecatinteraction == 2"
                         ),
            rglwidgetOutput("onecat3d4", width = 600),
            HTML("Depiction of simple slopes facilitate the understanding that body weight 
                  differentially predicts blood pressure in smokers and non-smokers. The slope
                   is larger in smokers than in non-smokers.
                  Viewing the simple slopes in their correct location on the surface 
                  reinforces the perspective developed in 2x2 anova and permits the 
                 conclusion that simple main effects and simple slopes are not different concepts.")
          ),  
          
          
          #onecat 3D data plus interaction surface plus simple slopes
          conditionalPanel(
            condition = ("input.ivtypes == 'onecat' &
                         input.onecatplot == 4 &
                         input.onecatinteraction == 3"
            ),
            tags$img(src="zerocat_pequod1.png", width=500),tags$br()
              ),  
          
          
          ########################
          ## two numerics
          conditionalPanel(
            condition = ("input.ivtypes == 'zerocat' &
                         input.twonumericplot == 1"),
            # data only
            rglwidgetOutput("twonumeric1", height = 400, width = 600)
            ),
          conditionalPanel(
            condition = ("input.ivtypes == 'zerocat' &
                         input.twonumericplot == 2"),
            # data only
            rglwidgetOutput("twonumeric2", height = 400, width = 600)
            ),
          conditionalPanel(
            condition = ("input.ivtypes == 'zerocat' &
                         input.twonumericplot == 3 &
                         input.twonumericinteraction == 1"),
            # data only
            rglwidgetOutput("twonumeric3", height = 400, width = 600)
            ),            
          conditionalPanel(
              condition = ("input.ivtypes == 'zerocat' &
                           input.twonumericplot == 3 &
                           input.twonumericinteraction == 2"),
              # data only
              rglwidgetOutput("twonumeric4", height = 400, width = 600)
              ),   
          conditionalPanel(
            condition = ("input.ivtypes == 'zerocat' &
                         input.twonumericplot == 3 &
                         input.twonumericinteraction == 3"),
            tags$img(src="zerocat_pequod1.png", width=500),tags$br()
            )           
          
          ),
        # finish second tabpanel
        tabPanel("About The Data Sets",
                  includeMarkdown('aboutdata.md')
         ),# finish third tabpanel
         tabPanel("About The App",
                  includeMarkdown('aboutapp.md')
         )# finish abouttheapp tabpanel
      )# finish tabsetPanel
        )# finish mainpanel
        )# finish sidebarlayout
                         )) #finish fluidpage and shinyui

