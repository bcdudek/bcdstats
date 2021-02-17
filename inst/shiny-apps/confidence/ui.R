
##### neeed to add shinyBS popover for text explanation.
# Define UI for dataset viewer application
shinyUI(
  pageWithSidebar(
    # Application title.
    titlePanel("Confidence Interval for a Sample Mean: A simulation"),
    
    sidebarPanel( width=4, align='left',
                  #class="span10",
                  wellPanel(
                  helpText("This app randomly samples N data points from a Normal Distribution.  ",
                           "The user specifies N.  Sample means are computed for each simulated sample.")
                  ),
                  wellPanel(
                    radioButtons("display1", label=strong("Choose wich Graphs"),
                                 c("Confidence Interval Graph Only"="cionly",
                                   "Confidence Interval Graph Plus Sampling Distribution of the Mean"="ciplussampl"))
                  ),
                  wellPanel(
                    div(class="row",
                        div(class="col-xs-6",
                            radioButtons("display2", label=strong("Display Choice for CI Graph"),
                                         c("Means Plus CI"="xbarplusci",
                                           "Means Only"="xbar"))
                        ),
                        div(class="col-xs-6",
                            selectInput("numsim", label = strong("Number of Simulated Samples"), 
                                        choices = list("One" = 1, "Two" = 2, "Five" = 5, "One Hundred" = 100), 
                                        selected = 5,
                                        width='85%'))
                    ),
                    div(class="row",
                        div(class="col-xs-6",
                            numericInput(inputId = "nsamp",
                                         label=strong("Sample Size"),
                                         value=4,
                                         min=2,
                                         max=200,
                                         width='85%')),
                        div(class="col-xs-6",
                            numericInput(inputId = "cilevel",
                                         label=strong("Confidence Level"),
                                         value=95,
                                         min=1,
                                         max=99,
                                         width='85%'))
                    ),
                    div(class="row",
                        div(class="col-xs-6",
                            numericInput(inputId = "mean",
                                         label = strong("Population Mean"),
                                         value = 0,
                                         min=1,
                                         max=1000,
                                         width='85%')),
                        div(class="col-xs-6",
                            numericInput(inputId = "stdev",
                                         label = strong("Population Std Dev"),
                                         value = 1,
                                         min=.01,
                                         max=100,
                                         width='85%'))
                    ),
                    actionButton("draw", "Resample"),
                    align="left"),
                  wellPanel(
                    radioButtons("sigma", label=strong("Population Variance"),
                                 c("Sigma Known (use Z Distribution)"="known",
                                   "Sigma not Known (use t Distribution)"="notknown"))
                  )
                  ,helpText("App created by Bruce Dudek")
    ),# finish sidebarpanel
    
    mainPanel(
      tags$head(
        tags$style(HTML("
                          .shiny-output-error-validation {
                          font-size: 20px; color: blue;
                          }
                          "))
      ),
      tabsetPanel(
        tabPanel("Plots",
                 helpText(" "),
                 helpText("Click your mouse on the plot to see an explanation of the graph."),
                 plotOutput('CIplot',width="90%", height="850px"),
                 bsPopover(id="CIplot", title="CI Plot Info",placement="left",
                           content = paste0("This app is a visual/conceptual demonstration of confidence intervals. ", 
                                            "A number random samples are simulated.  In the upper plot, the means of each sample ",
                                            "are depicted with a point.   Each sample has a confidence interval ",
                                            "computed, and centered on the sample mean for that respective sample.",
                                            "The horizontal lines represent the size of the confidence interval.",
                                            "If the interval overlaps the true value of the population mean ",
                                            "(the solid vertical line), then the interval is drawn as a black line.",
                                            "If the interval does not overlap the true value of the population mean, ",
                                            "then the interval is drawn red.  It is instructive to try to count ",
                                            "the number of red intervals while manipulating the sample size.",
                                            "If shown, the lower plot is the simulated sampling distribution ",
                                            "of the mean. <br> <br>  ",
                                            "If Sigma is known, all simulated intervals are the same width.  ",
                                            "If Sigma is not known, the intervals vary in width.  ",
                                            "Can you think of why this is the case?"),trigger="click")
        ),
        tabPanel("About",
                 includeMarkdown('about.md')
                 )
      ))
  ))
