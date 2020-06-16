# function to render markdown text for about panel and other things
#renderRmd <- function(path, input){
#  return(renderText( {
#    if (!require(knitr))
#      stop("knitr package is not installed")
#    if (!require(markdown))
#      stop("Markdown package is not installed")
#    # shiny:::dependsOnFile(path)
#    contents <- paste(readLines(path, warn = FALSE), collapse = '\n')
#    myenvir <- new.env() # Perhaps use parent.frame() ?!
#    assign('input', input, envir=myenvir)
#    html <- knitr::knit2html(text = contents, fragment.only = TRUE, envir=myenvir)
#    Encoding(html) <- 'UTF-8'
#    return(HTML(html))
#  }))
#}

####
#graphical display of the beta values

# mu 1 would be the null hypothesis mean and mu2 would be the alternative hypothesis mean

# set sem as the std error of the mean, the std dev of the sampling distributon of the mean
# choose alpha, the type I error rate as "type1"; it is set to .05 by default
# one tail, upper, both distribs plotted
alphabeta2 <- function(mu1,mu2,sem,type1){
  mu11 <- mu1
  mu21 <- mu2
  sem1 <- sem
  type11 <- type1
  if(mu1 < mu2){
    values1 <- seq(mu1-(5*sem),mu2+(5*sem),.005)
  }
  if(mu1 == mu2){
    values1 <- seq(mu1-(5*sem),mu2+(5*sem),.005)
  }
  if(mu2 < mu1){
    values1 <- seq(mu2-(5*sem),mu1+(5*sem),.005)
  }
  
  probs1 <- dnorm(values1, mu1, sem)
  cv <- mu1+((qnorm(type1,lower.tail=F))*sem)
  bta <- round(pnorm(cv,mu2,sem),digits=4)
  plot(values1, probs1, axes = F, type = "n", 
       cex.axis=1.2,cex.lab=1.4, cex.main=1.2,
       xlab = substitute(paste("Possible Values of ", bar(x))), 
       ylab = "Probability Density",
       main = "Null and Alternative Sampling Distributions of the Mean")
  axis(1, pos = 0)
  axis(2)
  #values2 <- values1 - mu1 +mu2
  values2 <- values1
  probs2 <- dnorm(values2, mu2, sem)  
  h <- dnorm(mu1,mu1,sem)    
  cex <- 0.8
  zoffset <- -.02
  quantile=1-type1
  xx <- qnorm(quantile,mu1,sem)
  #  lines(x,dnorm(x,mu1,sem),type='h',col=1)  
  u <- seq(xx,mu1+5*sem,length=601)
  lines(u,dnorm(u,mu1,sem),type="h",col="red")
  text(xx, zoffset * h,paste("rejection region"),adj=-.25,col=2,cex=1.0)
  v=seq(mu2-5*sem,xx,length=601)
  lines(v,dnorm(v,mu2,sem),type="h",col="lightblue")
  abline(0,0,col=1)
  abline(v=cv,col="red",lwd=2,lty=2)
  abline(v=mu1,col="black",lwd=.6)
  abline(v=mu2,col="black",lwd=.6)
  lines(values1, probs1, col = "black",lwd=1.4)
  lines(values2, probs2, col = "black",lwd=1.4)  
  mtext(substitute(paste( mu[0]==mu11, ", ", mu[A]==mu21, ", ",sigma[bar(x)]==sem1, ", ", 
                          alpha==type11, ", ", beta==bta, ",  c.v. "==cv)),col="blue",cex=1.3)
  #  lines(values1, probs1, col = "black")
  #text(x, h,paste(" Beta=",round(pnorm(x,mu2,sem),digits=4)),col="blue",cex=1)
  return(invisible())
  
}
# example:
#alphabeta2(mu1=100,mu2=97,sem=3,type1=.05)

# reworking to make one-tail for lower tail, both distribs plotted
alphabeta3 <- function(mu1,mu2,sem,type1){
  mu11 <- mu1
  mu21 <- mu2
  sem1 <- sem
  type11 <- type1
  if(mu1 < mu2){
    values1 <- seq(mu1-(5*sem),mu2+(5*sem),.005)
  }
  if(mu1 == mu2){
    values1 <- seq(mu1-(5*sem),mu2+(5*sem),.005)
  }
  if(mu2 < mu1){
    values1 <- seq(mu2-(5*sem),mu1+(5*sem),.005)
  }
  #  values1 <- seq(mu2-(4*sem),mu1+(4*sem),.005)
  probs1 <- dnorm(values1, mu1, sem)
  cv <- mu1+((qnorm(type1,lower.tail=T))*sem)
  bta <- round(pnorm(cv,mu2,sem,lower.tail=F),digits=4)
  plot(values1, probs1, axes = F, type = "n", 
       cex.axis=1.2,cex.lab=1.4, cex.main=1.2,
       xlab = substitute(paste("Possible Values of ", bar(x))), 
       ylab = "Probability Density",
       main = "Null and Alternative Sampling Distributions of the Mean")
  axis(1, pos = 0)
  axis(2)
  #values2 <- values1 - mu1 +mu2
  values2 <- values1
  probs2 <- dnorm(values2, mu2, sem)  
  h <- dnorm(mu1,mu1,sem)    
  cex <- 0.8
  zoffset <- -.02
  alpha=type1
  xx <- qnorm(alpha,mu1,sem)
  #  lines(x,dnorm(x,mu1,sem),type='h',col=1)  
  u <- seq(mu1-5*sem,xx,length=601)
  text(xx, zoffset * h,paste("rejection region"),adj=1.1,col="red",cex=1.0)
  lines(u,dnorm(u,mu1,sem),type="h",col="red")
  v=seq(xx,mu2+5*sem,length=601)
  lines(v,dnorm(v,mu2,sem),type="h",col="lightblue")
  abline(0,0,col=1)
  abline(v=cv,col="red",lwd=2,lty=2)
  abline(v=mu1,col="black",lwd=.6)
  abline(v=mu2,col="black",lwd=.6)
  lines(values1, probs1, col = "black",lwd=1.4)
  lines(values2, probs2, col = "black",lwd=1.4)  
  mtext(substitute(paste( mu[0]==mu11, ", ", mu[A]==mu21, ", ",sigma[bar(x)]==sem1, ", ", 
                          alpha==type11, ", ", beta==bta, ",  c.v. "==cv)),col="blue",cex=1.3)
  #  lines(values1, probs1, col = "black")
  #text(x, h,paste(" Beta=",round(pnorm(x,mu2,sem),digits=4)),col="blue",cex=1)
  return(invisible())
  
}
# example:
#alphabeta3(mu1=100,mu2=97,sem=3,type1=.05)
############two tail plot, show both distribs
alphabeta4 <- function(mu1,mu2,sem,type1){
  mu11 <- mu1
  mu21 <- mu2
  sem1 <- sem
  type11 <- type1
  if(mu1 < mu2){
    values1 <- seq(mu1-(5*sem),mu2+(5*sem),.005)
  }
  if(mu1 == mu2){
    values1 <- seq(mu1-(5*sem),mu2+(5*sem),.005)
  }
  if(mu2 < mu1){
    values1 <- seq(mu2-(5*sem),mu1+(5*sem),.005)
  }
  probs1 <- dnorm(values1, mu1, sem)
  cvupper <- mu1+((qnorm((type1/2),lower.tail=F))*sem)
  cvlower <- mu1+((qnorm((type1/2),lower.tail=T))*sem)
  #  bta <- round(pnorm(cvupper,mu2,sem),digits=4)
  bta <- round(((pnorm(cvupper,mu2,sem))-(pnorm(cvlower,mu2,sem))),digits=4)
  plot(values1, probs1, axes = F, type = "n", 
       cex.axis=1.2,cex.lab=1.4, cex.main=1.2,
       xlab = substitute(paste("Possible Values of ", bar(x))), 
       ylab = "Probability Density",
       main = "Null and Alternative Sampling Distributions of the Mean")
  axis(1, pos = 0)
  axis(2)
  #values2 <- values1 - mu1 +mu2
  values2 <- values1
  probs2 <- dnorm(values2, mu2, sem)  
  h <- dnorm(mu1,mu1,sem)    
  cex <- 0.8
  zoffset <- -.02
  quantileupper=1-(type1/2)
  xx <- qnorm(quantileupper,mu1,sem)
  #  lines(x,dnorm(x,mu1,sem),type='h',col=1)  
  u <- seq(xx,mu1+5*sem,length=601)
  lines(u,dnorm(u,mu1,sem),type="h",col="red")

  quantilelower=(type1/2)
  xxlower <- qnorm(quantilelower,mu1,sem)
  #  lines(x,dnorm(x,mu1,sem),type='h',col=1)  
  ulower <- seq(mu1-5*sem,xxlower,length=601)
  lines(ulower,dnorm(ulower,mu1,sem),type="h",col="red")
  
  text(xx, zoffset * h,paste("rejection region"),adj=-.25,col=2,cex=1.0)
  text(xx, h,substitute(paste("upper C.V.")),adj=-.15,col=2,cex=1.1)
  text(xx, .95*h,substitute(paste("= ",cvupper)),adj=-.15,col=2,cex=1.1)
  
  text(xxlower, zoffset * h,paste("rejection region"),adj=1.2,col=2,cex=1.0)
  text(xxlower, h,substitute(paste("lower C.V.")),adj=1.1,col=2,cex=1.1)
  text(xxlower, .95*h,substitute(paste("= ",cvlower)),adj=1.1,col=2,cex=1.1)
  
  v=seq(xxlower,xx,length=601)
  lines(v,dnorm(v,mu2,sem),type="h",col="lightblue")
  abline(0,0,col=1)
  abline(v=cvupper,col="red",lwd=2,lty=2)
  abline(v=cvlower,col="red",lwd=2,lty=2)
  abline(v=mu1,col="black",lwd=.6)
  abline(v=mu2,col="black",lwd=.6)
  lines(values1, probs1, col = "black",lwd=1.4)
  lines(values2, probs2, col = "black",lwd=1.4)  
  mtext(substitute(paste( mu[0]==mu11, ", ", mu[A]==mu21, ", ",sigma[bar(x)]==sem1, ", ", 
                          alpha==type11, ", ", beta==bta)),col="blue",cex=1.3)
  #  lines(values1, probs1, col = "black")
  #text(x, h,paste(" Beta=",round(pnorm(x,mu2,sem),digits=4)),col="blue",cex=1)
  return(invisible())
  
}
# example:
#alphabeta4(mu1=100,mu2=97,sem=3,type1=.05)


#######################################################################################
# plots to "remove" the null hypothesis curve from the visualization.
# one tail upper, nonull
alphabeta2b <- function(mu1,mu2,sem,type1){
  mu11 <- mu1
  mu21 <- mu2
  sem1 <- sem
  type11 <- type1
  if(mu1 < mu2){
    values1 <- seq(mu1-(5*sem),mu2+(5*sem),.005)
  }
  if(mu1 == mu2){
    values1 <- seq(mu1-(5*sem),mu2+(5*sem),.005)
  }
  if(mu2 < mu1){
    values1 <- seq(mu2-(5*sem),mu1+(5*sem),.005)
  }
  
  probs1 <- dnorm(values1, mu1, sem)
  cv <- mu1+((qnorm(type1,lower.tail=F))*sem)
  bta <- round(pnorm(cv,mu2,sem),digits=4)
  plot(values1, probs1, axes = F, type = "n",
       cex.axis=1.2,cex.lab=1.4, cex.main=1.2,
       xlab = substitute(paste("Possible Values of ", bar(x))), 
       ylab = "Probability Density",
       main = "Alternate Hypoth. Sampling Distribution of the Mean")
  axis(1, pos = 0)
  axis(2)
  #values2 <- values1 - mu1 +mu2
  values2 <- values1
  probs2 <- dnorm(values2, mu2, sem)  
  h <- dnorm(mu1,mu1,sem)    
  cex <- 0.8
  zoffset <- -.02
  quantile=1-type1
  xx <- qnorm(quantile,mu1,sem)
  #  lines(x,dnorm(x,mu1,sem),type='h',col=1)  
  u <- seq(xx,mu1+5*sem,length=601)
  #lines(u,dnorm(u,mu1,sem),type="h",col="red")
  text(xx, zoffset * h,paste("rejection region"),adj=-.25,col=2,cex=1.0)
  v=seq(mu2-5*sem,xx,length=601)
  lines(v,dnorm(v,mu2,sem),type="h",col="lightblue")
  abline(0,0,col=1)
  abline(v=cv,col="red",lwd=2,lty=2)
#  abline(v=mu1,col="black",lwd=.6)
  abline(v=mu2,col="black",lwd=.6)
#  lines(values1, probs1, col = "black",lwd=1.4)
  lines(values2, probs2, col = "black",lwd=1.4)  
  mtext(substitute(paste( mu[0]==mu11, ", ", mu[A]==mu21, ", ",sigma[bar(x)]==sem1, ", ", 
                          alpha==type11, ", ", beta==bta, ",  c.v. "==cv)),col="blue",cex=1.3)
  #  lines(values1, probs1, col = "black")
  #text(x, h,paste(" Beta=",round(pnorm(x,mu2,sem),digits=4)),col="blue",cex=1)
  return(invisible())
  
}
# example:
#alphabeta2b(mu1=100,mu2=104,sem=3,type1=.05)
# reworking to make one-tail for lower tail, nonull.
alphabeta3b <- function(mu1,mu2,sem,type1){
  mu11 <- mu1
  mu21 <- mu2
  sem1 <- sem
  type11 <- type1
  if(mu1 < mu2){
    values1 <- seq(mu1-(5*sem),mu2+(5*sem),.005)
  }
  if(mu1 == mu2){
    values1 <- seq(mu1-(5*sem),mu2+(5*sem),.005)
  }
  if(mu2 < mu1){
    values1 <- seq(mu2-(5*sem),mu1+(5*sem),.005)
  }
  #  values1 <- seq(mu2-(4*sem),mu1+(4*sem),.005)
  probs1 <- dnorm(values1, mu1, sem)
  cv <- mu1+((qnorm(type1,lower.tail=T))*sem)
  bta <- round(pnorm(cv,mu2,sem,lower.tail=F),digits=4)
  plot(values1, probs1, axes = F, type = "n", 
       cex.axis=1.2,cex.lab=1.4, cex.main=1.2,
       xlab = substitute(paste("Possible Values of ", bar(x))), 
       ylab = "Probability Density",
       main = "Alternate Hypoth. Sampling Distribution of the Mean")
  axis(1, pos = 0)
  axis(2)
  #values2 <- values1 - mu1 +mu2
  values2 <- values1
  probs2 <- dnorm(values2, mu2, sem)  
  h <- dnorm(mu1,mu1,sem)    
  cex <- 0.8
  zoffset <- -.02
  alpha=type1
  xx <- qnorm(alpha,mu1,sem)
  #  lines(x,dnorm(x,mu1,sem),type='h',col=1)  
  u <- seq(mu1-5*sem,xx,length=601)
  text(xx, zoffset * h,paste("rejection region"),adj=1.1,col=2,cex=1.0)
  #  lines(u,dnorm(u,mu1,sem),type="h",col="red")
  v=seq(xx,mu2+5*sem,length=601)
  lines(v,dnorm(v,mu2,sem),type="h",col="lightblue")
  abline(0,0,col=1)
  abline(v=cv,col="red",lwd=2,lty=2)
  #  abline(v=mu1,col="black",lwd=.6)
  abline(v=mu2,col="black",lwd=.6)
  #  lines(values1, probs1, col = "black",lwd=1.4)
  lines(values2, probs2, col = "black",lwd=1.4)  
  mtext(substitute(paste( mu[0]==mu11, ", ", mu[A]==mu21, ", ",sigma[bar(x)]==sem1, ", ", 
                          alpha==type11, ", ", beta==bta, ",  c.v. "==cv)),col="blue",cex=1.3)
  #  lines(values1, probs1, col = "black")
  #text(x, h,paste(" Beta=",round(pnorm(x,mu2,sem),digits=4)),col="blue",cex=1)
  return(invisible())
  
}
# example:
#alphabeta3b(mu1=100,mu2=97,sem=3,type1=.05)

############two tail plot, nonull
alphabeta4b <- function(mu1,mu2,sem,type1){
  mu11 <- mu1
  mu21 <- mu2
  sem1 <- sem
  type11 <- type1
  if(mu1 < mu2){
    values1 <- seq(mu1-(5*sem),mu2+(5*sem),.005)
  }
  if(mu1 == mu2){
    values1 <- seq(mu1-(5*sem),mu2+(5*sem),.005)
  }
  if(mu2 < mu1){
    values1 <- seq(mu2-(5*sem),mu1+(5*sem),.005)
  }
  probs1 <- dnorm(values1, mu1, sem)
  cvupper <- mu1+((qnorm((type1/2),lower.tail=F))*sem)
  cvlower <- mu1+((qnorm((type1/2),lower.tail=T))*sem)
  #  bta <- round(pnorm(cvupper,mu2,sem),digits=4)
  bta <- round(((pnorm(cvupper,mu2,sem))-(pnorm(cvlower,mu2,sem))),digits=4)
  plot(values1, probs1, axes = F, type = "n", 
       cex.axis=1.2,cex.lab=1.4, cex.main=1.2,
       xlab = substitute(paste("Possible Values of ", bar(x))), 
       ylab = "Probability Density",
       main = "Alternate Hypoth. Sampling Distribution of the Mean")
  axis(1, pos = 0)
  axis(2)
  #values2 <- values1 - mu1 +mu2
  values2 <- values1
  probs2 <- dnorm(values2, mu2, sem)  
  h <- dnorm(mu1,mu1,sem)    
  cex <- 0.8
  zoffset <- -.02
  quantileupper=1-(type1/2)
  xx <- qnorm(quantileupper,mu1,sem)
  #  lines(x,dnorm(x,mu1,sem),type='h',col=1)  
  u <- seq(xx,mu1+5*sem,length=601)
#  lines(u,dnorm(u,mu1,sem),type="h",col="red")
  
  quantilelower=(type1/2)
  xxlower <- qnorm(quantilelower,mu1,sem)
  #  lines(x,dnorm(x,mu1,sem),type='h',col=1)  
  ulower <- seq(mu1-5*sem,xxlower,length=601)
#  lines(ulower,dnorm(ulower,mu1,sem),type="h",col="red")
  
  text(xx, zoffset * h,paste("rejection region"),adj=-.25,col=2,cex=1.0)
  text(xx, h,substitute(paste("upper C.V.")),adj=-.15,col=2,cex=1.1)
  text(xx, .95*h,substitute(paste("= ",cvupper)),adj=-.15,col=2,cex=1.1)
  
  text(xxlower, zoffset * h,paste("rejection region"),adj=1.2,col=2,cex=1.0)
  text(xxlower, h,substitute(paste("lower C.V.")),adj=1.1,col=2,cex=1.1)
  text(xxlower, .95*h,substitute(paste("= ",cvlower)),adj=1.1,col=2,cex=1.1)
  
  v=seq(xxlower,xx,length=601)
  lines(v,dnorm(v,mu2,sem),type="h",col="lightblue")
  abline(0,0,col=1)
  abline(v=cvupper,col="red",lwd=2,lty=2)
  abline(v=cvlower,col="red",lwd=2,lty=2)
  abline(v=mu1,col="black",lwd=.6)
  abline(v=mu2,col="black",lwd=.6)
#  lines(values1, probs1, col = "black",lwd=1.4)
  lines(values2, probs2, col = "black",lwd=1.4)  
  mtext(substitute(paste( mu[0]==mu11, ", ", mu[A]==mu21, ", ",sigma[bar(x)]==sem1, ", ", 
                          alpha==type11, ", ", beta==bta)),col="blue",cex=1.3)
  #  lines(values1, probs1, col = "black")
  #text(x, h,paste(" Beta=",round(pnorm(x,mu2,sem),digits=4)),col="blue",cex=1)
  return(invisible())
  
}
# example:
#alphabeta4b(mu1=100,mu2=97,sem=3,type1=.05)

#######################################################################################
# plots to "remove" the alternative hypothesis curve from the visualization.
# one tail upper, noalt
alphabeta2c <- function(mu1,mu2,sem,type1){
  mu11 <- mu1
  mu21 <- mu2
  sem1 <- sem
  type11 <- type1
  if(mu1 < mu2){
    values1 <- seq(mu1-(5*sem),mu2+(5*sem),.005)
  }
  if(mu1 == mu2){
    values1 <- seq(mu1-(5*sem),mu2+(5*sem),.005)
  }
  if(mu2 < mu1){
    values1 <- seq(mu2-(5*sem),mu1+(5*sem),.005)
  }
  
  probs1 <- dnorm(values1, mu1, sem)
  cv <- mu1+((qnorm(type1,lower.tail=F))*sem)
  bta <- round(pnorm(cv,mu2,sem),digits=4)
  plot(values1, probs1, axes = F, type = "n",
       cex.axis=1.2,cex.lab=1.4, cex.main=1.2,
       xlab = substitute(paste("Possible Values of ", bar(x))), 
       ylab = "Probability Density",
       main = "Null Hypoth. Sampling Distribution of the Mean")
  axis(1, pos = 0)
  axis(2)
  #values2 <- values1 - mu1 +mu2
  values2 <- values1
  probs2 <- dnorm(values2, mu2, sem)  
  h <- dnorm(mu1,mu1,sem)    
  cex <- 0.8
  zoffset <- -.02
  quantile=1-type1
  xx <- qnorm(quantile,mu1,sem)
  #  lines(x,dnorm(x,mu1,sem),type='h',col=1)  
  u <- seq(xx,mu1+5*sem,length=601)
  lines(u,dnorm(u,mu1,sem),type="h",col="red")
  text(xx, zoffset * h,paste("rejection region"),adj=-.25,col=2,cex=1.0)
  v=seq(mu2-5*sem,xx,length=601)
  #lines(v,dnorm(v,mu2,sem),type="h",col="lightblue")
  abline(0,0,col=1)
  abline(v=cv,col="red",lwd=2,lty=2)
  abline(v=mu1,col="black",lwd=.6)
  #abline(v=mu2,col="black",lwd=.6)
  lines(values1, probs1, col = "black",lwd=1.4)
  #lines(values2, probs2, col = "black",lwd=1.4)  
  mtext(substitute(paste( mu[0]==mu11, ", ", mu[A]==mu21, ", ",sigma[bar(x)]==sem1, ", ", 
                          alpha==type11, ", ", beta==bta, ",  c.v. "==cv)),col="blue",cex=1.3)
  #  lines(values1, probs1, col = "black")
  #text(x, h,paste(" Beta=",round(pnorm(x,mu2,sem),digits=4)),col="blue",cex=1)
  return(invisible())
  
}
# example:
#alphabeta2b(mu1=100,mu2=104,sem=3,type1=.05)

# reworking to make one-tail for lower tail, no alternate.
alphabeta3c <- function(mu1,mu2,sem,type1){
  mu11 <- mu1
  mu21 <- mu2
  sem1 <- sem
  type11 <- type1
  if(mu1 < mu2){
    values1 <- seq(mu1-(5*sem),mu2+(5*sem),.005)
  }
  if(mu1 == mu2){
    values1 <- seq(mu1-(5*sem),mu2+(5*sem),.005)
  }
  if(mu2 < mu1){
    values1 <- seq(mu2-(5*sem),mu1+(5*sem),.005)
  }
  #  values1 <- seq(mu2-(4*sem),mu1+(4*sem),.005)
  probs1 <- dnorm(values1, mu1, sem)
  cv <- mu1+((qnorm(type1,lower.tail=T))*sem)
  bta <- round(pnorm(cv,mu2,sem,lower.tail=F),digits=4)
  plot(values1, probs1, axes = F, type = "n", 
       cex.axis=1.2,cex.lab=1.4, cex.main=1.2,
       xlab = substitute(paste("Possible Values of ", bar(x))), 
       ylab = "Probability Density",
       main = "Null Hypoth. Sampling Distributions of the Mean")
  axis(1, pos = 0)
  axis(2)
  #values2 <- values1 - mu1 +mu2
  values2 <- values1
  probs2 <- dnorm(values2, mu2, sem)  
  h <- dnorm(mu1,mu1,sem)    
  cex <- 0.8
  zoffset <- -.02
  alpha=type1
  xx <- qnorm(alpha,mu1,sem)
  #lines(x,dnorm(x,mu1,sem),type='h',col=1)  
  u <- seq(mu1-5*sem,xx,length=601)
  text(xx, zoffset * h,paste("rejection region"),adj=1.1,col=2,cex=1.0)
  lines(u,dnorm(u,mu1,sem),type="h",col="red")
  v=seq(xx,mu2+5*sem,length=601)
  #lines(v,dnorm(v,mu2,sem),type="h",col="lightblue")
  abline(0,0,col=1)
  abline(v=cv,col="red",lwd=2,lty=2)
  abline(v=mu1,col="black",lwd=.6)
  #abline(v=mu2,col="black",lwd=.6)
  lines(values1, probs1, col = "black",lwd=1.4)
  #lines(values2, probs2, col = "black",lwd=1.4)  
  mtext(substitute(paste( mu[0]==mu11, ", ", mu[A]==mu21, ", ",sigma[bar(x)]==sem1, ", ", 
                          alpha==type11, ", ", beta==bta, ",  c.v. "==cv)),col="blue",cex=1.3)
  #  lines(values1, probs1, col = "black")
  #text(x, h,paste(" Beta=",round(pnorm(x,mu2,sem),digits=4)),col="blue",cex=1)
  return(invisible())
  
}
# example:
#alphabeta3b(mu1=100,mu2=97,sem=3,type1=.05)

############two tail plot, noalt
alphabeta4c <- function(mu1,mu2,sem,type1){
  mu11 <- mu1
  mu21 <- mu2
  sem1 <- sem
  type11 <- type1
  if(mu1 < mu2){
    values1 <- seq(mu1-(5*sem),mu2+(5*sem),.005)
  }
  if(mu1 == mu2){
    values1 <- seq(mu1-(5*sem),mu2+(5*sem),.005)
  }
  if(mu2 < mu1){
    values1 <- seq(mu2-(5*sem),mu1+(5*sem),.005)
  }
  probs1 <- dnorm(values1, mu1, sem)
  cvupper <- mu1+((qnorm((type1/2),lower.tail=F))*sem)
  cvlower <- mu1+((qnorm((type1/2),lower.tail=T))*sem)
  #cv <- mu1+((qnorm(type1,lower.tail=F))*sem)
#  bta <- round(pnorm(cvupper,mu2,sem),digits=4)
  bta <- round(((pnorm(cvupper,mu2,sem))-(pnorm(cvlower,mu2,sem))),digits=4)
  plot(values1, probs1, axes = F, type = "n", 
       cex.axis=1.2,cex.lab=1.4, cex.main=1.2,
       xlab = substitute(paste("Possible Values of ", bar(x))), 
       ylab = "Probability Density",
       main = "Null Hypoth. Sampling Distribution of the Mean")
  axis(1, pos = 0)
  axis(2)
  #values2 <- values1 - mu1 +mu2
  values2 <- values1
  probs2 <- dnorm(values2, mu2, sem)  
  h <- dnorm(mu1,mu1,sem)    
  cex <- 0.8
  zoffset <- -.02
  quantileupper=1-(type1/2)
  xx <- qnorm(quantileupper,mu1,sem)
  #lines(x,dnorm(x,mu1,sem),type='h',col=1)  
  u <- seq(xx,mu1+5*sem,length=601)
  lines(u,dnorm(u,mu1,sem),type="h",col="red")
  
  quantilelower=(type1/2)
  xxlower <- qnorm(quantilelower,mu1,sem)
  #  lines(x,dnorm(x,mu1,sem),type='h',col=1)  
  ulower <- seq(mu1-5*sem,xxlower,length=601)
    lines(ulower,dnorm(ulower,mu1,sem),type="h",col="red")
  
  text(xx, zoffset * h,paste("rejection region"),adj=-.25,col=2,cex=1.0)
  text(xx, h,substitute(paste("upper C.V.")),adj=-.15,col=2,cex=1.1)
  text(xx, .95*h,substitute(paste("= ",cvupper)),adj=-.15,col=2,cex=1.1)
  
  text(xxlower, zoffset * h,paste("rejection region"),adj=1.2,col=2,cex=1.0)
  text(xxlower, h,substitute(paste("lower C.V.")),adj=1.1,col=2,cex=1.1)
  text(xxlower, .95*h,substitute(paste("= ",cvlower)),adj=1.1,col=2,cex=1.1)
  
  v=seq(xxlower,xx,length=601)
#  lines(v,dnorm(v,mu2,sem),type="h",col="lightblue")
  abline(0,0,col=1)
  abline(v=cvupper,col="red",lwd=2,lty=2)
  abline(v=cvlower,col="red",lwd=2,lty=2)
  abline(v=mu1,col="black",lwd=.6)
  abline(v=mu2,col="black",lwd=.6)
  lines(values1, probs1, col = "black",lwd=1.4)
#  lines(values2, probs2, col = "black",lwd=1.4)  
  mtext(substitute(paste( mu[0]==mu11, ", ", mu[A]==mu21, ", ",sigma[bar(x)]==sem1, ", ", 
                          alpha==type11, ", ", beta==bta)),col="blue",cex=1.3)
  #  lines(values1, probs1, col = "black")
  #text(x, h,paste(" Beta=",round(pnorm(x,mu2,sem),digits=4)),col="blue",cex=1)
  return(invisible())
  
}
# example:
#alphabeta4c(mu1=100,mu2=97,sem=3,type1=.05)

