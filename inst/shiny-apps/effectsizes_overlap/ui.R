# License: Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License
# License:  http://creativecommons.org/licenses/by-nc-sa/4.0/

library(shiny)
library(shinythemes)
library(shinyWidgets)
#library(shinyBS)
# provide javascript for sidebar scrollbar and overflow
js <- '
$(document).ready(function(){
  $("[id^=sw-content-]").on("shown", function(){
    $(".sidebar").css({"overflow-y": "visible"});
  }).on("hidden", function(){
    $(".sidebar").css({"overflow-y": "auto"});
  });
});
'
shinyUI(navbarPage(theme = shinytheme('flatly'), # cosmo, flatly, paper, superhero??
  id = 'navbarpage',
  title = "Effect Size and Distribution Overlap",
  tabPanel(
    #withMathJax(),
    'Plot',
    withMathJax(),
    #shinyUI(fluidPage(theme = shinytheme('flatly'), # cosmo, flatly, paper, superhero??
    chooseSliderSkin("Shiny", color="steelblue"),
    
    # siderbar inputs
    sidebarLayout(
      sidebarPanel(
        tags$head(tags$script(HTML(js))),
        style = "height: 90vh; overflow-y: auto;", 
        checkboxInput('info', "Show Some Background Info", value=FALSE),
        conditionalPanel(
          condition = "input.info",
          h5("This app facilitates understanding of an independent samples comparison.  Contemplate comparing two hypothetical population distributions of a random (dependent) variable:  Perhaps a 'control' group with mu=100, and a 'treatment' group with a higher mean value controlled by choice of a Cohen's d value.  The standard deviation (sigma) within the populations is set to 15.  The app permits viewing the degree of overlap of the two distributions depending on choice of the Cohen's d value.  Details on effect sizes and overlap indices are in the Formulas/Explanations/References tab."),
        ),
        #br().
        sliderInput('cohend',
                    "Choose Cohen's d value:",
                    min = 0,
                    max = 5,
                    value = .5,
                    step  = .1
        ),
      ),
      # Show a plot of the generated distribution
      mainPanel(
        br(),
        plotOutput("distPlot"),
        #textOutput("text1"),
        br(),
        HTML("What can this demonstration tell you?"),
        HTML("<ul>
        <li>The app provides an intuitive visual understanding of how the effect sizes 
            and degree of overlap change with larger or smaller differences between 
            \\(\\mu_2\\) and  \\(\\mu_1\\).</li>
        <li>Information on how the numeric indices can be used to formalize 
            this visual impression can be found in the 
            Formulas/Explanations/References tab.</li>
        </ul>"),
      ),# end mainpanel
    ), # end sidebarlayout
  ), # end tabpanel plot
  tabPanel(
    'Formulas/Explanations/References',
    includeMarkdown('comments.md')
  ),  # end tabpanel formulas
  tabPanel(
    'About',
    includeMarkdown('about.md')
  )
))
