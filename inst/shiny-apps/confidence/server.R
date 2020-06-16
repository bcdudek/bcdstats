library(shiny)
library(shinyBS)
shinyServer(function(input, output, session){
  source("tools.R")
  source("ttest2.R")
  numsimnum <- reactive({as.numeric(input$numsim)})
  drawseed <- reactive({input$draw})


  
  makeplotz <- function(nsim,n, pop.mean , pop.sd, conf.lvl, displaytype1,displaytype2,drawseed){
    set.seed(drawseed+1)
    
    validate(
      need(is.wholenumber(input$nsamp), 'Shiny would like you to enter whole numbers for the sample size.')
    )
    validate(
      need(input$nsamp >=2 & input$nsamp <= 200, ' For this visualization, Shiny works best with a maximum sample size of 200 and a minimum of 2.')
    )
    
    validate(
      need(is.wholenumber(input$cilevel), 'Shiny would like you to enter whole numbers for the confidence level.')
    )
    validate(
      need(input$nsamp >=1 & input$cilevel <= 99, ' For this visualization, Shiny works best with a maximum of 99 and a minimum of 1 for the Confidence Level.')
    )
    
    validate(
      need(input$mean >=-10000 & input$mean <= 10000, ' For this visualization, Shiny works best with a maximum mean of 10000 and a minimum of -10000.')
    )

    validate(
      need(input$stdev >=-.1 & input$stdev <= 1000000, ' For this visualization, Shiny works best with a maximum standard deviation of 1000000 and a minimum of 0.1.')
    )
    
    par(mfrow=c(2,1))
    plot(NULL,
         #       xlim = c(pop.mean-(5*(pop.sd/(n^.5))),pop.mean+(5*(pop.sd/(n^.5)))),
         xlim = c(pop.mean-5*pop.sd,pop.mean+5*pop.sd), ylim = c(0,nsim),
         yaxt = 'n',
         xlab = expression(mu),
         ylab = paste(nsim, 'Replications/Simulations'),
         cex.lab=1.35
         #        ,main = "Confidence Intervals of the mean from Simulated Samples"
    )
    
    abline(v = pop.mean, col = 'grey')
    mtext(paste(conf.lvl,"% CI Simulation"), cex = 1.25, line=1, at = pop.mean)
    
    
    ci2 <- conf.lvl/100
    multiplier <- (qnorm((1-ci2)/2, lower.tail=F))
    
    means <- rep(NA,nsim)
    upper <- rep(NA,nsim)
    lower <- rep(NA,nsim)
    
    for (i in 1:nsim){
      xbarsz <- vector(mode="numeric",length=nsim)
      means[i] <- mean(rnorm(n,pop.mean,pop.sd))
      upper[i] <- means[i] + (multiplier*(pop.sd/(n^.5)))
      lower[i] <- means[i] - (multiplier*(pop.sd/(n^.5)))
      
      if((pop.mean > lower[i]) & (pop.mean < upper[i])){
        if(displaytype2 == "xbarplusci"){
          lines(c(lower[i],upper[i]),c(i,i), lwd=1.2,col='black')
          points(means[i],i,pch=21,col="black",bg="grey")}
        else if(displaytype2=="xbar"){
          points(means[i],i,pch=21,col="black",bg="grey")  
        }
      }
      else{
        #lines(c(interval[1],interval[2]),c(i,i), lwd=2,col='red' )
        if(displaytype2 == "xbarplusci"){      
          lines(c(lower[i],upper[i]),c(i,i), lwd=1.2,col='red' )
          points(means[i],i,pch=21,col="black",bg="red")}
        else if(displaytype2=="xbar"){
          points(means[i],i,pch=21,col="black",bg="red")}
      }
      xbarsz <- means
      #print(xbars)
    }
    if(displaytype1=="ciplussampl"){
      hist(xbarsz,
           xlim = c(pop.mean-5*pop.sd,pop.mean+5*pop.sd),
           main="Simulated Sampling Distribution of the Mean",
           ylab="")
      rug(xbarsz,col="blue")}
    par(mfrow=c(1,1))
  }
  
  makeplott <- function(nsim,n, pop.mean , pop.sd, conf.lvl, displaytype1,displaytype2,drawseed){
    set.seed(drawseed+1)
    
    validate(
      need(is.wholenumber(input$nsamp), 'Shiny would like you to enter whole numbers for the sample size.')
    )
    validate(
      need(input$nsamp >=3 & input$nsamp <= 200, ' For this visualization, Shiny works best with a maximum sample size of 200 and a minimum of 3.')
    )
    
    validate(
      need(is.wholenumber(input$cilevel), 'Shiny would like you to enter whole numbers for the confidence level.')
    )
    validate(
      need(input$nsamp >=1 & input$cilevel <= 99, ' For this visualization, Shiny works best with a maximum of 99 and a minimum of 1 for the Confidence Level.')
    )
    
    validate(
      need(input$mean >=-10000 & input$mean <= 10000, ' For this visualization, Shiny works best with a maximum mean of 10000 and a minimum of -10000.')
    )
    
    validate(
      need(input$stdev >=-.1 & input$stdev <= 1000000, ' For this visualization, Shiny works best with a maximum standard deviation of 1000000 and a minimum of 0.1.')
    )
    
    par(mfrow=c(2,1))
    plot(NULL,
         #       xlim = c(pop.mean-(5*(pop.sd/(n^.5))),pop.mean+(5*(pop.sd/(n^.5)))),
         xlim = c(pop.mean-5*pop.sd,pop.mean+5*pop.sd), ylim = c(0,nsim),
         xlab = expression(mu),
         #xlab = paste(conf.lvl,"% CI"),
         yaxt='n',
         ylab = paste(nsim, 'Replications/Simulations'),
         cex.lab=1.35
         #        ,main = "Confidence Intervals of the mean from Simulated Samples"
    )
    
    abline(v = pop.mean, col = 'blue')
    mtext(paste(conf.lvl,"% CI Simulation"), cex = 1.25, line=1, at = pop.mean)
    #mtext(expression(mu), cex = 1.75, line=1, at = pop.mean)
    
    
    ci2 <- conf.lvl/100
    multiplier <- (qt((1-ci2)/2, df=n-1, lower.tail=F))

    
    meanst <- rep(NA,nsim)
    #upper <- rep(NA,nsim)
    #lower <- rep(NA,nsim)

    
    for (i in 1:nsim){
      xbarst <- vector(mode="numeric",length=nsim)  
      estx1 <- vector(mode="numeric",length=nsim)  
       xscores <- rnorm(n, mean = pop.mean, pop.sd)
       meanst[i] <- mean(xscores)
        fit1 <- t.test2(xscores,conf.level=ci2)
        estx <- fit1$estimate
        estx1 <- as.numeric(estx[1])
        interval <- fit1$conf.int
        #print(meanst)

      if((pop.mean > interval[1]) & (pop.mean < interval[2])){
        if(displaytype2 == "xbarplusci"){
          lines(c(interval[1],interval[2]),c(i,i), lwd=1.2,col='black')
          points(estx[1],i,pch=21,col="black",bg="grey")
          }
        else if(displaytype2=="xbar"){
          points(estx[1],i,pch=21,col="black",bg="grey")
        }
      }
      else{
        if(displaytype2 == "xbarplusci"){
          lines(c(interval[1],interval[2]),c(i,i), lwd=1.2,col='red')
          points(estx[1],i,pch=21,col="black",bg="red")
          }
        else if(displaytype2=="xbar"){
          points(estx[1],i,pch=21,col="black",bg="red")
          }          #points(means2[i],i,pch=21,col="black",bg="red")}
      }
        #print(as.numeric(estx[1]))

        xbarst <- meanst
        #print(xbarst)
    }  # ends for loop

    if(displaytype1=="ciplussampl"){
      hist(xbarst,
           xlim = c(pop.mean-5*pop.sd,pop.mean+5*pop.sd),
           main="Simulated Sampling Distribution of the Mean",
           ylab="")
      rug(xbarst,col="blue")}

    par(mfrow=c(1,1))
  } # ends makeplott function  
  
  output$CIplot <-renderPlot({
    if(input$sigma=="known"){
      makeplotz(numsimnum(), input$nsamp, input$mean, input$stdev, input$cilevel,
             input$display1,input$display2,drawseed())
    }
    else if(input$sigma=="notknown"){
      makeplott(numsimnum(), input$nsamp, input$mean, input$stdev, input$cilevel,
                input$display1,input$display2,drawseed())
    }
  })
})

#  initial for loop for t dist based plot
#    for (i in 1:nsim){
# xbars <- vector(mode="numeric",length=nsim)
# means2 <- vector(mode="numeric",length=nsim)
# stdev2 <- vector(mode="numeric",length=nsim)
# samples <- vector(mode="numeric",length=nsim)
# samples <- rnorm(n,pop.mean,pop.sd)
# means2[i] <- mean(samples[i])
#stdev2[i] <- sd(samples[i])
# # print(means2)
# #print(stdev2)

#means[i] <- mean(rnorm(n,pop.mean,pop.sd))
# upper[i] <- means2[i] + (multiplier*(pop.sd/(n^.5)))
# lower[i] <- means2[i] - (multiplier*(pop.sd/(n^.5)))


# if((pop.mean > lower[i]) & (pop.mean < upper[i])){
#   if(displaytype2 == "xbarplusci"){
#     lines(c(lower[i],upper[i]),c(i,i), lwd=1.2,col='black')
#     points(means2[i],i,pch=21,col="black",bg="grey")}
#   else if(displaytype2=="xbar"){
# #     points(means2[i],i,pch=21,col="black",bg="grey")  
#   }
# }
# else{
#   #lines(c(interval[1],interval[2]),c(i,i), lwd=2,col='red' )
#   if(displaytype2 == "xbarplusci"){      
#     lines(c(lower[i],upper[i]),c(i,i), lwd=1.2,col='red' )
#     points(means2[i],i,pch=21,col="black",bg="red")}
#   else if(displaytype2=="xbar"){
#     points(means2[i],i,pch=21,col="black",bg="red")}
# }
# xbars <- means2
# #print(xbars)
# }
