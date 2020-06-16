# the functions pgt and qgt are the primary plotting functions for
# drawing the distributions under the two "control" conditions
# pgt for entering quantile and returning prob and qgt the reverse
pgt <-
  function(quantile,df=100,tail="upper")
  {
    if(df==1)
    {
      xlim <- c(-65,65)
    }
    else if(df >=2& df <3)
    {
      xlim <- c(-15,15)
    }
    else if(df>=3 & df <4)
    {
      xlim <- c(-10,10)
    }
    else if(df>=4 & df < 5)
    {
      xlim <- c(-8,8)
    }
    else if(df>=5 & df <6)
    {
      xlim <- c(-6,6)
    }
    else if(df>=6 & df < 7)
    {
      xlim <- c(-5,5)
    }
    else if(df >= 7 & df <8)
    {
      xlim <- c(-5,5)
    }
    else if(df >=8 & df < 9)
    {
      xlim <- c(-4.8,4.8)
    }
    else if(df >=9 & df<10)
    {
      xlim <- c(-4.6,4.6)
    }
    else if(df==10)
    {
      xlim <- c(-4.5,4.5)
    }
    else if(df>=10)
    {
      xlim <- c(-4,4)
    }
    if(tail=="upper")
    {
      xmin <- qt(.0005, df=df)
      xmax <- qt(.9995, df=df)
      .x <- seq(xmin,xmax,length=10000)
      hx <- dt(.x,df)
      ub=quantile
      cvupper=round(ub,5)
      prob <- pt(quantile,df=df,lower.tail=F)
      par(cex.main=1.5,cex.axis=1.5,font.lab=2, mar = c(5, 6, 4, 5) + 0.1, 
          mgp = c(3.5, 1, 0),las=1,bty="n")
      plot(.x, hx, type="n", xlab="t Values", ylab="Density",
           main=paste("t Distribution, df=",df), axes=T,
           xlim=xlim,
           cex.lab=1.5,
           cex.axis=1.5)
      j <- .x >=ub
      lines(.x, hx)
      abline(h=0)
      abline(v=0)
      abline(v=cvupper,col="red")
      polygon(c(ub,.x[j],xmax), c(0,hx[j],0), col="lightgray")
      result <- paste("prob(t \u2265 ",cvupper," ) =",
                      pdisplay(prob,digits.scientific=4))
      mtext(result,3,cex=1.5) 
    }
    else if(tail=="lower")
    {
      xmin <- qt(.0005, df=df)
      xmax <- qt(.9995, df=df)
      .x <- seq(xmin,xmax,length=10000)
      hx <- dt(.x,df)
      #ub=quantile
      lb=quantile
      #cvupper=round(ub,5)
      cvlower=round(lb,5)
      prob <- pt(quantile,df=df,lower.tail=T)
      par(cex.main=1.5,cex.axis=1.5,font.lab=2, mar = c(5, 6, 4, 5) + 0.1, 
          mgp = c(3.5, 1, 0),las=1,bty="n")
      plot(.x, hx, type="n", xlab="t Values", ylab="Density",
           main=paste("t Distribution, df=",df), axes=T,
           xlim=xlim,
           cex.lab=1.5,
           cex.axis=1.5)
      i <- .x <= lb
      lines(.x, hx)
      abline(h=0)
      abline(v=0)
      abline(v=cvlower,col="red")
      polygon(c(xmin,.x[i],lb), c(0,hx[i],0), col="lightgray")
      result <- paste("prob(t \u2264 ",cvlower," ) =",
                      pdisplay(prob,digits.scientific=4))
      mtext(result,3,cex=1.5) 
    }
    
    else if(tail=="two")
    {
      xmin <- qt(.0005, df=df)
      xmax <- qt(.9995, df=df)
      .x <- seq(xmin,xmax,length=10000)
      hx <- dt(.x,df)
      ub=quantile
      lb=-quantile
      cvupper=round(ub,5)
      cvlower=round(lb,5)
      prob <- (pt(ub,df=df,lower.tail=F))+(pt(lb,df=df,lower.tail=T))
      par(cex.main=1.5,cex.axis=1.5,font.lab=2, mar = c(5, 6, 4, 5) + 0.1, 
          mgp = c(3.5, 1, 0),las=1,bty="n")
      plot(.x, hx, type="n", xlab="t Values", ylab="Density",
           main=paste("t Distribution, df=",df), axes=T,
           xlim=xlim,
           cex.lab=1.5,
           cex.axis=1.5)
      i <- .x <= lb
      j <- .x >=ub
      lines(.x, hx)
      abline(h=0)
      abline(v=0)
      abline(v=cvupper,col="red")
      abline(v=cvlower,col="red")
      polygon(c(xmin,.x[i],lb), c(0,hx[i],0), col="lightgray")
      polygon(c(ub,.x[j],xmax), c(0,hx[j],0), col="lightgray")
      area <- pt(ub,df=df) - pt(lb,df=df)
      result <- paste("prob(t \u2264 ",cvlower," or  t \u2265",cvupper,") =",
                      pdisplay(prob,digits.scientific=4))
      mtext(result,3,cex=1.5) 
    }
    
  }
# pgt(-2.1,df=10,tail="lower")

qgt <-
  function(prob,df=1,tail="upper")
  {
    if(df==1)
    {
      xlim <- c(-65,65)
    }
    else if(df >=2& df <3)
    {
      xlim <- c(-15,15)
    }
    else if(df>=3 & df <4)
    {
      xlim <- c(-10,10)
    }
    else if(df>=4 & df < 5)
    {
      xlim <- c(-8,8)
    }
    else if(df>=5 & df <6)
    {
      xlim <- c(-6,6)
    }
    else if(df>=6 & df < 7)
    {
      xlim <- c(-5,5)
    }
    else if(df >= 7 & df <8)
    {
      xlim <- c(-5,5)
    }
    else if(df >=8 & df < 9)
    {
      xlim <- c(-4.8,4.8)
    }
    else if(df >=9 & df<10)
    {
      xlim <- c(-4.6,4.6)
    }
    else if(df==10)
    {
      xlim <- c(-4.5,4.5)
    }
    else if(df>=10)
    {
      xlim <- c(-4,4)
    }
    if(tail=="upper")
    {
      xmin <- qt(.0005, df=df)
      xmax <- qt(.9995, df=df)
      .x <- seq(xmin,xmax,length=10000)
      hx <- dt(.x,df)
      ub=qt(prob,df,lower.tail=F)
      cvupper=round(ub,5)
      par(cex.main=1.5,cex.axis=1.5,font.lab=2, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0),las=1,bty="n")
      plot(.x, hx, type="n", xlab="t Values", ylab="Density",
           main=paste("t Distribution, df=",df), axes=T,
           xlim=xlim,
           cex.lab=1.5,
           cex.axis=1.5)
      j <- .x >=ub
      lines(.x, hx)
      abline(h=0)
      abline(v=0)
      abline(v=cvupper,col="red")
      polygon(c(ub,.x[j],xmax), c(0,hx[j],0), col="lightgray")
      result <- paste("prob(t \u2265 ",cvupper,") =",
                      pdisplay(prob,digits.scientific=4))
      mtext(result,3,cex=1.5) 
    }
    else if(tail=="lower")
    {
      xmin <- qt(.0005, df=df)
      xmax <- qt(.9995, df=df)
      .x <- seq(xmin,xmax,length=10000)
      hx <- dt(.x,df)
      ub=qt(prob,df,lower.tail=F)
      lb=qt(prob,df,lower.tail=T)
      cvupper=round(ub,5)
      cvlower=round(lb,5)
      par(cex.main=1.5,cex.axis=1.5,font.lab=2, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0),las=1,bty="n")
      plot(.x, hx, type="n", xlab="t Values", ylab="Density",
           main=paste("t Distribution, df=",df), axes=T,
           xlim=xlim,
           cex.lab=1.5,
           cex.axis=1.5)
      i <- .x <= lb
      lines(.x, hx)
      abline(h=0)
      abline(v=0)
      abline(v=cvlower,col="red")
      polygon(c(xmin,.x[i],lb), c(0,hx[i],0), col="lightgray")
      result <- paste("prob(t \u2264 ",cvlower," ) =",
                      pdisplay(prob,digits.scientific=4))
      mtext(result,3,cex=1.5) 
    }
    
    else if(tail=="two")
    {
      xmin <- qt(.0005, df=df)
      xmax <- qt(.9995, df=df)
      .x <- seq(xmin,xmax,length=10000)
      hx <- dt(.x,df)
      ub=qt(prob/2,df,lower.tail=F)
      lb=qt(prob/2,df,lower.tail=T)
      cvupper=round(ub,5)
      cvlower=round(lb,5)
      par(cex.main=1.5,cex.axis=1.5,font.lab=2, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0),las=1,bty="n")
      plot(.x, hx, type="n", xlab="t Values", ylab="Density",
           main=paste("t Distribution, df=",df), axes=T,
           xlim=xlim,
           cex.lab=1.5,
           cex.axis=1.5)
      i <- .x <= lb
      j <- .x >=ub
      lines(.x, hx)
      abline(h=0)
      abline(v=0)
      abline(v=cvupper,col="red")
      abline(v=cvlower,col="red")
      polygon(c(xmin,.x[i],lb), c(0,hx[i],0), col="lightgray")
      polygon(c(ub,.x[j],xmax), c(0,hx[j],0), col="lightgray")
      area <- pt(ub,df=df) - pt(lb,df=df)
      result <- paste("prob(t \u2264 ",cvlower," or  t \u2265 ",cvupper,") =",
                      pdisplay(prob,digits.scientific=4))
      mtext(result,3,cex=1.5) 
    }
    
  }
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


# function for displaying pvalues with paste function on graphs
# written by Jason Bryer sep 2014
pdisplay <- function(x, threshold=5, 
                     digits=threshold, 
                     nsmall=digits,
                     digits.scientific=nsmall,
                     nsmall.scientific=(digits.scientific-1)) {
  if(digits < threshold) { 
    warning('Setting digits less than threshold may yield unexpected results')
  }
  results <- sapply(x, function(x1) {
    result <- ''
    if(x1 <= 1 / (10 ^ threshold)) {
      while((x1 * (10 ^ threshold)) < 1) {
        threshold <- threshold + 1
      }
      result <- paste0(format(x1 * (10 ^ threshold), digits=digits.scientific, 
                              drop0trailing=FALSE, nsmall=nsmall.scientific),
                       'e-', threshold)
    } else {
      result <- format(round(x1, digits=digits),
                       digits=digits,
                       drop0trailing=FALSE, 
                       scientific=FALSE, 
                       nsmall=(nsmall))
    }
    return(result)
  })
  return(results)
}

# examples of pdisplay usage
#if(FALSE) {
#  pdisplay(2112.0)
#  pdisplay(.000002112)
#  pdisplay(.000002112112112)
#  pdisplay(.000000201)
#  
#  pdisplay(pnorm(3, lower.tail=FALSE))
#  pdisplay(pnorm(3.8, lower.tail=FALSE))
#  pdisplay(pnorm(4.75, lower.tail=FALSE))
#  pdisplay(pnorm(4.8, lower.tail=FALSE), digits.scientific=4)
#}


#------------------------------------------------------------------------------------------------------
# function to test if value is an integer so that df can be checked for non whole number values
#NOT USED. DIDNT WORK WITH SHINY prob apps for df limitation
# used until change basic plotting function to set scales for ranges of df instead of exact integers
# from http://stackoverflow.com/questions/3476782/check-if-the-number-is-integer
#check.integer <- function(N){
#  !grepl("[^[:digit:]]", format(40, scientific = FALSE, digits = 20))
#}

# INSTEAD, USED THIS FROM THE as.integer help page
is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
