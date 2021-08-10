# License: Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License
# License:  http://creativecommons.org/licenses/by-nc-sa/4.0/
# core overlapping plots based on:
# https://ggplot2tutor.com/tutorials/sampling_distributions
library(shiny)
library(ggplot2)
#library(rstatix)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # validateinput <- reactive({
  #   validate(
  #     need(
  #       is.wholenumber(input$nSims),
  #       'Shiny would like you to enter whole numbers for the number of simulations.'
  #     )
  #   )
  #   validate(
  #     need(
  #       input$nSims >= 1 &
  #         input$nSims <= 15000,
  #       ' For this visualization, Shiny has set a maximum of 15000 simulations and a minimum of 1.'
  #     )
  #   )
  # })
  
  output$distPlot <- renderPlot({
    #validateinput()
    cd <- input$cohend
    sigma <- 15
    mu1 <- 100
    mu2 <- cd*sigma + 100
    mu2r <- round(mu2,1)
    cl <- (pnorm(cd/(2^.5)))
    u3 <- 100*(pnorm(cd))
    u2 <- pnorm(cd/2)
    u1 <- 100*(((2*u2)-1)/u2) # note that cohen's U1 is non-overlap, not overlap
    u1ovl <- u1 #100-u1
    rom <- 100-(100*(2*pnorm(-cd/2))) # roms index was overlap. I changed to non-overlap
    etasq <- (cd^2)/(cd^2 +4)
    etasq2 <- round(etasq,2)
    meandiff <- round(mu2-mu1,2)
    temp <- paste("mu[2] ==", mu2r) 
    p1 <- ggplot(data.frame(x = c(20, mu2+(3.67*sigma))), aes(x)) +
      #p1 <- ggplot(data.frame(x = c(mu1-(3.67*sigma), mu2+(3.67*sigma))), aes(x)) +
      stat_function(fun = dnorm,
                    geom = "area",
                    fill = 'darkgrey', # was #2E8B57
                    alpha=.5,
                    args = list(
                      mean = mu1,
                      sd = sigma
                    )) +
      stat_function(fun = dnorm,
                    geom = "area",
                    fill = "steelblue", # was orange3
                    alpha = .6,
                    args = list(
                      mean = mu2,
                      sd = sigma
                    )) +
      #geom_vline(xintercept=mu1)+
      #geom_vline(xintercept=mu2)+
      geom_segment(x=mu1,xend=mu1,y=0,yend=.0275)+
      geom_segment(x=mu2,xend=mu2,y=0,yend=.0275)+
      xlab("DV Scale") + ylab("Density") +
      theme_classic()+
      theme(axis.title = element_text(size = 14))+
      theme(axis.text = element_text(size = 14)) +
      theme(axis.title.x = element_text(hjust=.6))
    
    p1b <- p1 +
      ylim(0,.031)+
      scale_x_continuous(breaks = seq(20,  mu2+(3.67*sigma), by = 20))
    #scale_y_continuous(breaks = seq(0,  .035, by = .01))
    #annotate("segment", x = mu1, xend = mu2, y = .0265, yend = .0265,
    #         colour = "blue", size = .5, 
    #         arrow = arrow(ends="both", angle=25,length=unit(.3, "cm"))) 
    p2 <-
      p1b + annotate(
        "text",
        x = 20,
        y = .027,
        hjust = 0,
        label = "Effect Sizes:",
        size=5
      ) +
      annotate(
        "text",
        x = 24,
        y = .025,
        hjust = 0,
        label = paste("• Cohen's d =", round(cd,2)),
        size=5
      ) +
      annotate(
        "text",
        x = mu2,
        y = .0295,
        hjust = 0,
        label = temp,
        size=5,
        parse=TRUE
      ) +
      annotate(
        "text",
        x = 24,
        y = .023,
        hjust = 0,
        label = paste("• eta squared =", round(etasq,2)),
        size=5
      ) +
      annotate(
        "text",
        x = 24,
        y = .021,
        hjust = 0,
        label = paste("• Mean Difference =", meandiff),
        size=5
      ) 

    p3 <-
      p2 + annotate(
        "text",
        x = 20,
        y = .019,
        hjust = 0,
        label = "Overlap Indices:",
        size=5
      ) +
      annotate(
        "text",
        x = 24,
        y = .017,
        hjust = 0,
        label = paste("• Cohen's U1 =", round(u1ovl,2),"%"),
        size=5
      ) +
      annotate(
        "text",
        x = 24,
        y = .015,
        hjust = 0,
        label = paste("• Rom's non-overlap =", round(rom,2), "%"),
        size=5
      ) +
      annotate(
        "text",
        x = 24,
        y = .013,
        hjust = 0,
        label = paste("• CL (prob superiority) =", round(cl,4)),
        size=5
      ) +
      annotate(
        "text",
        x = 24,
        y = .011,
        hjust = 0,
        label = paste("• Cohen's U3 =", round(u3,2),"%"),
        size=5
      )   
    p3
  })
  # output$text1    <- renderText(
  #   paste('The app provides intuitive understanding of how the effect sizes 
  #         and degree of overlap change with increasing difference between 
  #         \\(\\mu_2\\) and  \\(\\mu_2\\).  Information on how the numeric 
  #         indices can be used to formalize this visual impression can be 
  #         found in the Formulas/Explanations/References tab. ',sep = '')
  # )  
})