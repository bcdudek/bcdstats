# based on: https://github.com/Lakens/shiny_apps/tree/master/MOOC/assignment_1
# License: Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License
# License:  http://creativecommons.org/licenses/by-nc-sa/4.0/

library(shiny)
library(shinythemes)
library(shinyWidgets)
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
shinyUI(navbarPage(#theme = shinytheme('flatly'), # cosmo, flatly, paper, superhero??
  id = 'navbarpage',
  title = "P-Values Distribution",
  tabPanel(
   # withMathJax(),
    'Plot',
    withMathJax(),
#shinyUI(fluidPage(theme = shinytheme('flatly'), # cosmo, flatly, paper, superhero??
                  chooseSliderSkin("Shiny", color="seagreen"),
                  # siderbar inputs
                  sidebarLayout(
                    sidebarPanel(
                      tags$head(tags$script(HTML(js))),
                      h5("This app simulates one-sample t-tests.  Data were generated from a population distribution with specified mu and sigma, Each sample mean is tested against the null hypothesis population mu of 100.  The figure plots the p-value distribution."),
                      style = "height: 90vh; overflow-y: auto;", 
                      numericInput('nSims',
                                   'Number of Simulations:',
                                   min = 10,
                                   max = 1e5,
                                   value = 5000, 
                                   step = 10),
                      numericInput('n',
                                   'Enter the sample size:',
                                   min = 4,
                                   max = 100000,
                                   value = 25,
                                   step = 1),
                      sliderInput('mu',
                                  'Choose the hypothesized (alternative) population mean (\\(\\mu\\)):',
                                  min = 80,
                                  max = 120,
                                  value = 107.5,
                                  step  = .5
                      ),
                      sliderInput('sigma',
                                  'Choose the std dev (\\(\\sigma\\)) of the population:',
                                  min = 1,
                                  max = 30,
                                  value = 15,
                                  step  = 1
                      ),
                      sliderInput('bars',
                                  'Set the number of bars for the histogram:',
                                  min = 20,
                                  max = 100,
                                  value = 25,
                                  step  = 5
                      ),
                      sliderInput('alpha',
                                  'Choose the alpha level (Type I error rate):',
                                  min = 0.01,
                                  max = 0.10,
                                  value = 0.05,
                                  step  = 0.005
                      ),
                      # checkboxInput('zoom',
                      #               'Zoom graph to the p < 0.05 area.',
                      #               value = F
                      # ),
                      # br(),
                      # h5("Modified from a  Shiny app created by Daniel Lakens:", a("Lakens GitHub", href="https://github.com/Lakens/shiny_apps/tree/master/MOOC/assignment_1"))
                      ),
                    # Show a plot of the generated distribution
                    mainPanel(
                      br(),
                      plotOutput("distPlot"),
                      textOutput("text1"),
                      br(),
                      HTML("What can these Simulations tell you?"),
                      HTML("<ul><li>Notice how the app provides a way to understand power.</li>
                            <li>A very useful concept that emerges is the rectangular shape of the distribution when the mu chosen (the alternative population mean), matches the null value of 100.  When the null is true, all pvalues are equally likely!  This fact holds for all NHST tests.</li>
                            <li>When sample size changes, power also changes, but cohen's d does not.</li>
                            <li>Cohen's d is known to be upward biased, especially for smaller sample sizes.  This is why the average simulated value is probably larger than the theoretical value, except sometimes for when the number of simulations chosen is very small (which gives an imprecise estimate). The Hedges g statistic should give a more precise estimate of d.  Hedges g cannot be calculated when sample size is less than three.</li>
                            </ul>"),
                      br(),
                      h5(""),
                      br(),
                      h5(""),
                      br()
                    ),# end mainpanel
                  ), # end sidebarlayout
  ), # end tabpanel plot
tabPanel(
  'Comments/Formulas',
  includeMarkdown('comments.md')
),
tabPanel(
  'About',
  includeMarkdown('about.md')
)
))
