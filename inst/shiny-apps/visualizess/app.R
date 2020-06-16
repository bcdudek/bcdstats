library(shiny)
library(shinyBS)
library(ggplot2)
# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Visualize Sum of Squares and Variance"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        checkboxInput("high", "Show Variable with Higher Dispersion", TRUE),
        bsTooltip("null", "placeholder so that addPopover wil work",
                  "right", options = list(container = "body")),
        # this conditional panel controls figure components for the high disperson graph
        conditionalPanel(
          condition= "input.high",
          wellPanel(
          radioButtons("highfig", "Choose plot components:",
                       c("Show Only the Data" = "data",
                         "Show the mean as a dashed line" = "mean",
                         "Show the deviation from the mean for the largest value" = "xdev",
                         "Show the same X deviation in a vertical orientation" = "xdevvert",
                         "Show the Square implied by X deviation 'Squared'" ="square",
                         "Show the Squares for all Data Points" = "squares",
                         "Add a square for the average, the Sample Variance" = 'variance')
          
          ) # end radiobuttons
          ) # end wellpanel
        ),
        
        checkboxInput("low", "Show Variable with Lower Dispersion", FALSE),
        # this conditional panel controls figure components for the low disperson graph
        conditionalPanel(
          condition= "input.low",
          wellPanel(
            radioButtons("lowfig", "Choose plot components:",
                         c("Show Only the Data" = "data",
                           "Show the mean as a dashed line" = "mean",
                           "Show the deviation from the mean for the largest value" = "xdev",
                           "Show the same X deviation in a vertical orientation" = "xdevvert",
                           "show the Square implied by X deviation 'Squared'" ="square",
                           "show the Squares for all Data Points" = "squares",
                           "Add a square for the average, the Sample Variance" = 'variance')                         
            ) # end radiobuttons
          ) # end wellpanel
        ) # end conditional panel
        
      ),
      

      mainPanel(
        tabsetPanel(
          tabPanel("Plots",
        fluidRow(
          column(6,
                 #HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                     # Hover your mouse over the plots for more information."),
                 h5("      Click your mouse on the plots for more information."),
                 plotOutput("highfigplot")
         )),
        fluidRow(
          column(6,
                 br(),
                 plotOutput("lowfigplot")))
        ),#finish plottab
        tabPanel("Show the Data",
                 HTML("High Dispersion Variable"),
                  tableOutput("dtable"),
                 tableOutput("stattable"),
                 br(),
                 HTML("Low Dispersion Variable"),
                 tableOutput("dtable2"),
                 tableOutput("stattable2")
        ),
        tabPanel("Two Kinds of Variances",
                 withMathJax(),
                 p(h5("Visualizing the Variance.")),br(),
                 p("This app provides a way of visualizing the variance of a variable as a geometric entity.  We define 
				            the variance this way, as the average of areas of N squares, because the 'formula' for variance 
				          	involves a squared value.  Textbook definitions of Sample Variance label it in various ways, including: 
			           		\\(S^2\\) (an upper case S),  \\(SD^2\\). amd \\(VAR_X\\) "),
                 br(),
                 p("This sample variance is usally defined with this expression:  
                     \\(\\frac{{\\sum\\limits_{i = 1}^n {({X_i} - \\bar X)^2} }}{{N}}\\)
                   "),
                 p("Expansion of the numerator permits a rewrite:  
                     \\(\\frac{{\\sum\\limits_{i = 1}^n {({X_i} - \\bar X)({X_i} - \\bar X)} }}{{N}}\\)
                   "),
                 
                 p("Either of these expressions make it clear that the deviation of each X value from its mean is 
                    multiplied by itself, yielding the 'area of a geometric square'
                   perspective that the plots in this app emphasize.  When this is done for each X value and the N quantities are summed, 
                   that gives us the numerator of the expression.  This numerator is usally give a shorthand notation of SS.  This is because it is the 
                   'sum of squared deviations from the mean'.  So we can write the variance expression a third way:  
                   \\(\\frac{{SS}}{{N}}\\)"
                   ),
                 br(),
                 p("Students are encouraged to obtain the data values from the 'show the data' tab of this app and perform the calculations for these
                    expressions in Excel, or a with a calculator."),
                 br(),
                 p("Some users of this app may have been taught to compute the Variance a different way.  This expression is similar to the ones above, 
                    differing only in the denominator:    \\(\\frac{{\\sum\\limits_{i = 1}^n {({X_i} - \\bar X)^2} }}{{N-1}}\\)."),
                 br(),
                    p("The numerator is still called Sum of Squares (SS), but the expression is no longer the arithmetic average of the squared deviations.  
                    It is close, but will be slightly larger.
                   "),
                 p("This modified version of the expression is usually denoted as \\(s^2\\), with a lower case s.  It is the more commonly used 
                    variance expression.  This preference is because it is an unbiased estimate of the variance in the population from which the N X scores 
                    were randomly sampled.  Although it is no longer exactly the arithmetic average of areas of squares it is close, especially for larger 
                    sample sizes (N).
                   "),
                 p("Introductory Statistics coursework will expand on the difference in these two variance compuations.  
                    In this app we used the first of the two versions which is typicalled the sample variance.  
                    Since the 'area of a square' concept is related to the numerator, the distinction in the two 
                    expressions doesn't alter they way we understand Sum of Squares.
                   ")
                 ),
        tabPanel("About the App",
                 includeMarkdown('about.md')
        )
      )#finish tabsetpanel
     )#finish mainpanel
))
# Define server logic required to draw a histogram
server <- function(input, output, session) {
#################################################################
  # first for an n of five, with relatively large variance
  set.seed(25)
  x <-runif(5,min=82, max=118)+.9025
  y <- rep(0,5)
  y
  xbar <- mean(x)
  xdev <- x-xbar
  maxxdev <- abs(max(xdev))
  rangex <- max(x)-min(x)
  xdevsq <- xdev^2
  base <- as.data.frame(cbind(y,x,xdev))
  basehigh <- as.data.frame(cbind(x,xdev,xdevsq))
  colnames(basehigh) <- c("X","X Dev from Mean", "X Dev Squared")
  #str(base)   
  #sd(x)**2
  #(var(x)*4/5)**.5
  SD1 <- 10.89265
  SS <- SD1*5
  halfsd1 <- SD1/2
  sampsize <- 5
  highstats <- as.data.frame(cbind(xbar, SS, SD1))
  colnames(highstats) <- c("Mean of X","Sum of Squares", "Sample Variance")
#################################################################
  # now repeat the whole thing with a smaller variance
  set.seed(3)
  x2 <-runif(5,min=92,max=108)
  y2 <- rep(0,5)
  y2
  
  xbar2 <- mean(x2)
  xdev2 <- x2-xbar2
  xdevsq2 <- xdev2^2
  maxxdev2 <- abs(max(xdev2))
  rangex2 <- max(x2)-min(x2)
  
  base2 <- as.data.frame(cbind(y2,x2,xdev2))
  baselow <- as.data.frame(cbind(x2,xdev2,xdevsq2))
  colnames(baselow) <- c("X","X Dev from Mean", "X Dev Squared")
  # str(base2) 
  #sd(x2)**2
  #(var(x2)*4/5)**.5
  SD2 <- 3.572762
  SS2 <- SD2*5
  halfsd2 <- SD2/2
  lowstats <- as.data.frame(cbind(xbar2, SS2, SD2))
  colnames(lowstats) <- c("Mean of X","Sum of Squares", "Sample Variance")
################################################################  
  # plots for high dispersion data
  # Basic stripchart
  p <- ggplot(base, aes(x = x, y=y)) + 
    geom_dotplot(binwidth=.7) +
    xlim(min(x), max(x)) +
    ylim(0,rangex) + 
    labs(y=NULL) + labs(x="X") +
    theme(axis.text.y = element_blank())+
    theme(aspect.ratio=1)+
    theme(axis.title.x=element_text(size=rel(1.3)))+
    theme(axis.text.x=element_text(size=rel(1.5)))

  p2 <- p +  geom_vline(aes(xintercept=xbar),linetype=2)
  # depict the x deviation from the mean for the largest data point
  p3 <- p2 +
    geom_segment(x=max(x), xend=xbar,y=0,yend=0,colour="blue")
  # do the same xdeviation in the vertical orientation
  p4 <- p3 +
    geom_segment(x=max(x), xend=max(x),y=0,yend=maxxdev,colour="blue")
  #view it as a square
  p5 <- p4 +
    geom_segment(x=max(x), xend=xbar, y=maxxdev, yend=maxxdev,colour="blue")+
    geom_rect(xmin=mean(x),xmax=max(x), ymin=0,ymax=maxxdev,
              fill="skyblue",alpha=.05)
  # show all the squares
  p6 <- p2 +
    geom_segment(x=x, xend=xbar, y=0, yend=0,colour="blue")+ # bottom
    geom_segment(x=x, xend=x,y=0,yend=abs(xdev),colour="blue")+ #side
    geom_segment(x=x, xend=xbar, y=abs(xdev), yend=abs(xdev),colour="blue") + #top
    geom_segment(x=xbar, xend=xbar, y=0, yend=abs(xdev),colour="blue") + #mean
    geom_rect(xmin=x,xmax=xbar, ymin=0,ymax=abs(xdev),
              fill="skyblue",alpha=.05)
  # add a square for the average, the sample variance (SD squared)
  p7 <- p6 +
    geom_segment(x=xbar-halfsd1, xend=xbar+halfsd1, y=18, yend=18,colour="orange")+ # bottom
    geom_segment(x=xbar-halfsd1, xend=xbar+halfsd1, y=18+SD1,yend=18+SD1,colour="orange")+ #top
    geom_segment(x=xbar-halfsd1, xend=xbar-halfsd1, y=18, yend=18+SD1, colour="orange") + #left
    geom_segment(x=xbar+halfsd1, xend=xbar+halfsd1, y=18, yend=18+SD1, colour="orange") + #right
    geom_rect(xmin=xbar-halfsd1, xmax=xbar+halfsd1, ymin=18,ymax=18+SD1,
              fill="orange",alpha=.02)  
###############################################################
  # low dispersion plotting
  # Basic stripchart
  ps <- ggplot(base2, aes(x = x2, y=y2)) +
    geom_dotplot(binwidth=.7)+
    xlim(min(x),max(x)) +
    ylim(0,rangex) +
    labs(y=NULL, x="X") +
    theme(axis.text.y = element_blank())+
    theme(aspect.ratio=1)+
    theme(axis.title.x=element_text(size=rel(1.3)))+
    theme(axis.text.x=element_text(size=rel(1.5)))
  ps2 <- ps +  geom_vline(aes(xintercept=xbar2),linetype=2)
  # depict the x deviation from the mean for the largest data point
  ps3 <- ps2 +
    geom_segment(x=max(x2), xend=xbar2,y=0,yend=0,colour="blue")
  # do the same xdeviation in the vertical orientation
  ps4 <- ps3 +
    geom_segment(x=max(x2), xend=max(x2),y=0,yend=maxxdev2,colour="blue")
  #view it as a square
  ps5 <- ps4 +
    geom_segment(x=max(x2), xend=xbar2, y=maxxdev2, yend=maxxdev2,colour="blue")+
    geom_rect(xmin=mean(x2),xmax=max(x2), ymin=0,ymax=maxxdev2,
              fill="skyblue",alpha=.05)
  # show all the rectangles
  ps6 <- ps2 +
    geom_segment(x=x2, xend=xbar2, y=0, yend=0,colour="blue")+ # bottom
    geom_segment(x=x2, xend=x2,y=0,yend=abs(xdev2),colour="blue")+ #side
    geom_segment(x=x2, xend=xbar2, y=abs(xdev2), yend=abs(xdev2),colour="blue") + #top
    geom_segment(x=xbar2, xend=xbar2, y=0, yend=abs(xdev2),colour="blue") + #mean
    geom_rect(xmin=x2,xmax=xbar2, ymin=0,ymax=abs(xdev2),
              fill="skyblue",alpha=.05)
  ps7 <- ps6 +
    geom_segment(x=xbar2-halfsd2, xend=xbar2+halfsd2, y=18, yend=18,colour="orange")+ # bottom
    geom_segment(x=xbar2-halfsd2, xend=xbar2+halfsd2, y=18+SD2,yend=18+SD2,colour="orange")+ #top
    geom_segment(x=xbar2-halfsd2, xend=xbar2-halfsd2, y=18, yend=18+SD2, colour="orange") + #left
    geom_segment(x=xbar2+halfsd2, xend=xbar2+halfsd2, y=18, yend=18+SD2, colour="orange") + #right
    geom_rect(xmin=xbar2-halfsd2, xmax=xbar2+halfsd2, ymin=18,ymax=18+SD2,
              fill="orange",alpha=.02)  
  
##############################################################  
output$highfigplot <- renderPlot(
  if(input$high & input$highfig == 'data'){
    p
  }
  else if(input$high & input$highfig == 'mean'){
    p2
  }
  else if(input$high & input$highfig == 'xdev'){
    p3
  }
  else if(input$high & input$highfig == 'xdevvert'){
    p4
  }
  else if(input$high & input$highfig == 'square'){
    p5
  }
  else if(input$high & input$highfig == 'squares'){
    p6
  }
  else if(input$high & input$highfig == 'variance'){
    p7
  }
) # end renderplot  
  
  ###############################################################
  
  
  output$lowfigplot <- renderPlot(
    if(input$low & input$lowfig == 'data'){
      ps
    }
    else if(input$low & input$lowfig == 'mean'){
      ps2
    }
    else if(input$low & input$lowfig == 'xdev'){
      ps3
    }
    else if(input$low & input$lowfig == 'xdevvert'){
      ps4
    }
    else if(input$low & input$lowfig == 'square'){
      ps5
    }
    else if(input$low & input$lowfig == 'squares'){
      ps6
    }
    else if(input$low & input$lowfig == 'variance'){
      ps7
    }
  ) # end renderplot  

  observeEvent(input$high,{
    removePopover(session, 'highfigplot')
    if (input$high){ 
  addPopover(
    session,
    "highfigplot",
    "What Does the Plot Show?",
    content = "Squaring an X value deviation from its mean can be thought of as computing a
    the area of a square.  This figure shows those squares for all the X values.  
    When those areas are summed, we call it the Sum of Squares.  The average of these square areas
    is shown with the orange square and this average defines the Sample Variance, often denoted as S-squared or SD-squared.  <BR><BR>
    
    The user should compare the size of these squares for this 'high dispersion' variable with the 'low dispersion' variable shown 
    in the figure below.",
    trigger = 'click',placement="left"
  )
}#end if for high fig
})#end observeEvent

  observeEvent(input$low,{
    removePopover(session, 'lowfigplot')
  if (input$low){ 
  addPopover(
    session,
    "lowfigplot",
    "What Does the Plot Show?",
    content = "Notice in this figure, where the data points are less dispersed (closer to the mean), that
    the squares are smaller and their average (orange - the variance) is considerable smaller.
    <br><br>
    The user should compare the size of these squares for this 'low dispersion' variable with the 'high dispersion' variable shown 
    in the figure above.",
    trigger = 'click',placement="left"
  ) 
  }#end if for low fig
  })#end observeEvent
  
output$dtable <- renderTable({
  basehigh
})
  
output$dtable2 <- renderTable({
  baselow
})

output$stattable <- renderTable({
  highstats
})

output$stattable2 <- renderTable({
  lowstats
})

  }# end server

# Run the application 
shinyApp(ui = ui, server = server)

