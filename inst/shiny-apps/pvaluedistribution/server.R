# based on: https://github.com/Lakens/shiny_apps/tree/master/MOOC/assignment_1
# License: Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License
# License:  http://creativecommons.org/licenses/by-nc-sa/4.0/
library(shiny)
library(pwr)
library(ggplot2)
#library(rstatix)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  source("tools.R")
  validateinput <- reactive({
    validate(
      need(
        is.wholenumber(input$nSims),
        'Shiny would like you to enter whole numbers for the number of simulations.'
      )
    )
    validate(
      need(
        input$nSims >= 1 &
          input$nSims <= 15000,
        ' For this visualization, Shiny has set a maximum of 15000 simulations and a minimum of 1.'
      )
    )
    validate(
      need(
        is.wholenumber(input$n),
        'Shiny would like you to enter whole numbers for the sample size.'
      )
    )
    validate(need(
      input$n >= 3 &
        input$n <= 125,
      'Please enter a value for sample size greater than 1 and less than 125.'
    ))
  })

  output$distPlot <- renderPlot({
    validateinput()
    sims <- input$nSims
    M    <- input$mu
    SD   <- input$sigma
    a    <- input$alpha
    p    <- numeric(sims) #initialize a variable to store all simulated p-values
    d    <- numeric(sims) # initialize a variable to store all cohen's d's
    #t    <- numeric(sims) # initialize a variable to store all t values
    etasq    <- numeric(sims) # initialize a variable to store all eta squared's
    h <- numeric(sims)
    bars <- input$bars
    n    <- input$n
    df <- n-1
    dtheor <- (M-100)/15
    m <- df/2
    # approx hedges correction.
    #hcorrect <- 1-(3/((4*df)-1))
    hcorrect <- gamma(m)/((gamma(m-.5))*(m^.5)) # from hedges 1981
    # hedges g correction taken from:
    #https://www.real-statistics.com/students-t-distribution/one-sample-t-test/one-sample-effect-size/
    # trying to track down why in 1981 paper it is m-1, not .5.  one-sample difference?????
    #Run simulation
    for(i in 1:sims){ #for each simulated experiment
      x<-rnorm(n = n, mean = M, sd = SD) #Simulate data with specified mean, standard deviation, and sample size
      z<-t.test(x, mu=100,conf.level = 1 - a) #perform the t-test against mu (set to value you want to test against)
      d[i] <- (z$estimate-100)/(z$stderr*(n^.5)) # calc cohen's d's and save
      #h[i] <- (z$estimate-100)/(z$stderr*(n^.5))*(1-(3/((4*df)-1))) # calc hedges g approx
      h[i] <- (z$estimate-100)/(z$stderr*(n^.5))*hcorrect # calc hedges g
      etasq[i] <- (z$statistic^2)/(z$statistic^2 + df)# odd values
      #etasq[i] <- ((d[i]^2)/((d[i]^2)+4))
      p[i]<-z$p.value #get the p-value and store it
    }
    # avg cohen's d and hedges g
    davg <- round(sum(d)/sims,4)
    havg <- round(sum(h)/sims,4)
    eavg <- round(sum(etasq)/sims,4)
    etatheor <- round(dtheor^2/(dtheor^2 + 4),4)
    #Check power by summing significant p-values and dividing by number of simulations
    sigs <- sum((p < a)/sims) #power
    #Calculate power formally by power analysis
    power<-pwr.t.test(d=(M-100)/SD, n=n,sig.level= a,type="one.sample",alternative="two.sided")$power 
    #determines M when power > 0. When power = 0, will set  M = 100.
    #z <- input$zoom
    x_max <- 1
    y_max <- sims
    labels  <- seq(0,1,0.1)
    # if( z ){
    #   x_max<-0.05
    #   y_max<-sims/5
    #   labels <- seq(0,1,0.01)
    # }
    baseplot <- ggplot(data = as.data.frame(p), aes(p))
    plot1 <-
      baseplot + geom_histogram(
        binwidth = 1 / bars,
        fill = "seagreen",
        col = 'black',
        alpha = .6,
        boundary = 0
      ) +
      xlab('p-value scale') + ylab('Frequency of p-values') +
      coord_cartesian(xlim = c(0, x_max), ylim = c(0, y_max)) + theme_bw(base_size = 18) +
      ggtitle(
        paste(
          "Simulated p-value sampling distribution for ",
          format(sims, nsmall = 0),
          " one-sample,\n(two-sided) t-tests. These parameters produce ",
          format(power * 100, digits = 3),
          "% theoretical power.",
          sep = ""
        )
      ) +
      theme(plot.title = element_text(size = 14)) +
      geom_vline(
        xintercept = a,
        col = "seagreen",
        lwd = 1,
        alpha = .3
      ) +
      #geom_hline(yintercept = (0.05/a)*sims/bars, col="seagreen", lwd = 1, alpha=.3) +
      #annotate("text", x=.85, y=(0.05/a)*sims/bars, vjust = -1, label = "Type I error rate", col="seagreen")
      annotate(
        "text",
        x = a + .01,
        y = sims,
        hjust = 0,
        label = "Alpha Level",
        col = "seagreen"
      ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.title = element_blank(),
      legend.position = "top"
    ) +
    annotate(
        geom = "text",
        x = .55,
        y = .9 * sims,
        hjust=0,
        label = paste("Theoretical Cohen's d =", round(dtheor,3)),
        color = "black"
      ) +
      annotate(
        geom = "text",
        x = .55,
        y = .85 * sims,
        hjust=0,
        label = paste("Average Simulated Cohen's d =", round(davg,3)),
        color = "black"
      ) +  
      annotate(
        geom = "text",
        x = .55,
        y = .8 * sims,
        hjust = 0,
        label = paste("Average Simulated Hedge's g =", round(havg, 3)),
        color = "black"
      ) +
    #   annotate(
    #     geom = "text",
    #     x = .55,
    #     y = .75 * sims,
    #     hjust = 0,
    #     label = paste("Theoretical eta sqaured =", round(etatheor, 3)),
    #     color = "black"
    #   ) +
    #   annotate(
    #   geom = "text",
    #   x = .55,
    #   y = .70 * sims,
    #   hjust = 0,
    #   label = paste("Average Simulated eta sqaured =", round(eavg, 3)),
    #   color = "black"
    # ) +
       theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())

output$text1    <- renderText(
      paste(toString(round(100*sigs,2)),'% of the simulated t-tests were significant; compare this percentage to the theoretical power which is written in 
the title of the plot.',sep = '')
    )

plot1
  })


})