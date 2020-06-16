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
if(FALSE) {
  pdisplay(2112.0)
  pdisplay(.000002112)
  pdisplay(.000002112112112)
  pdisplay(.000000201)
  
  pdisplay(pnorm(3, lower.tail=FALSE))
  pdisplay(pnorm(3.8, lower.tail=FALSE))
  pdisplay(pnorm(4.75, lower.tail=FALSE))
  pdisplay(pnorm(4.8, lower.tail=FALSE), digits.scientific=4)
}

# functions for plotting chi squared distributions


qgchisq2 <- function (prob, df = 1, tail = "upper") 
{
  if (df == 1) {
    xmin <- qchisq(5e-04, df = 1, ncp = 0)
    xmax <- qchisq(0.999, df = 1, ncp = 0)
    .x <- seq(xmin, xmax, length = 1000)
    hx <- dchisq(.x, df = 1)
    ub = qchisq(prob, df = 1, lower.tail = F)
    cvupper = round(ub, 5)
    par(cex.main=1.5,cex.axis=1.5,font.lab=2, mar = c(5, 6, 4, 5) + 0.1, 
        mgp = c(3.8, 0.7, 0),las=1,bty="n")
        plot(.x, hx, type = "n", xlab = "Chi Square Values", 
         ylab = "Density (truncated Y axis)", ylim = c(0, 0.25), main = "Chi Square Distribution, df=1", 
         axes = T, xlim = c(0, xmax),
         cex.axis=1.5, cex.lab=1.4)
    j <- .x >= ub
    lines(.x, hx)
    segments(0,0,xmax,0)
    segments(0,0,0,max(hx))
    #abline(h = 0)
    #abline(v = 0)
    abline(v = ub, col = "red")
    polygon(c(ub, .x[j], xmax), c(0, hx[j], 0), col = "lightgrey")
    result <- paste("prob(Chi Sq \u2265 ", cvupper, ") =", 
                    pdisplay(prob,digits.scientific=4))
    mtext(result, 3,cex=1.5)
  }
  else if (df >= 1) {
    xmin2 <- qchisq(5e-04, df = df, ncp = 0)
    xmax2 <- qchisq(0.9995, df = df, ncp = 0)
    .x <- seq(xmin2, xmax2, length = 10000)
    hx <- dchisq(.x, df)
    ub = qchisq(prob, df, lower.tail = F)
    cvupper = round(ub, 5)
    par(cex.main=1.5,cex.axis=1.5,font.lab=2, mar = c(5, 6, 4, 5) + 0.1, 
        mgp = c(3.8, 0.7, 0),las=1,bty="n")
    plot(.x, hx, type = "n", xlab = "Chi Square Values", 
         ylab = "Density", main = paste("Chi Square Distribution, df=", 
                                        df), axes = T, xlim = c(0.9 * xmin2, xmax2),
         cex.axis=1.5,cex.lab=1.5)
    j <- .x >= ub
    lines(.x, hx)
    segments(0,0,xmax2,0)
    segments(0,0,0,max(hx))
    #abline(h = 0)
    #abline(v = 0)
    abline(v = ub, col = "red")
    polygon(c(ub, .x[j], xmax2), c(0, hx[j], 0), col = "lightgrey")
    result <- paste("prob(Chi Sq \u2265 ", cvupper, ") =", 
                    pdisplay(prob,digits.scientific=4))
    mtext(result, 3,cex=1.5)
  }
}

pgchisq2 <-
  function(quantile,df=1,tail="upper")
  {
    
    if(df==1)
    {
      xmin <- qchisq(.000005, df=1, ncp=0)
      xmax <- qchisq(.99995, df=1, ncp=0)
      .x <- seq(xmin,xmax,length=1000)
      hx <- dchisq(.x,df=1)
      ub=quantile
      cvupper=round(ub,5)
      prob <- pchisq(quantile,df=1,lower.tail=F)
      par(cex.main=1.5,cex.axis=1.5,font.lab=2, mar = c(5, 6, 4, 5) + 0.1, 
          mgp = c(3.8, 0.7, 0),las=1,bty="n")
      plot(.x, hx, type="n", xlab="Chi Square Values", ylab="Density (truncated Y axis)",
           ylim=c(0,.25),
           main="Chi Square Distribution, df=1", axes=T,xlim=c(0,xmax),
           cex.axis=1.5,cex.lab=1.4)
      j <- .x >=ub
      lines(.x, hx)
      segments(0,0,xmax,0)
      segments(0,0,0,max(hx))
      #abline(h=0)
      #abline(v=0)
      abline(v=quantile,col="red")
      polygon(c(ub,.x[j],xmax), c(0,hx[j],0), col="lightgrey")
      result <- paste("prob(Chi Sq \u2265 ", cvupper, ") =",
                      pdisplay(prob,digits.scientific=4))
      mtext(result,3,cex=1.5) 
    }
    
    else if(df>=1)
    {
      xmin2 <- qchisq(.0000005, df=df, ncp=0)
      xmax2 <- qchisq(.9999995, df=df, ncp=0)
      .x <- seq(xmin2,xmax2,length=10000)
      hx <- dchisq(.x,df)
      ub=quantile
      cvupper=round(ub,5)
      prob <- pchisq(quantile,df=df,lower.tail=F)
      par(cex.main=1.5,cex.axis=1.5,font.lab=2, mar = c(5, 6, 4, 5) + 0.1, 
          mgp = c(3.8, 0.7, 0),las=1,bty="n")
      plot(.x, hx, type="n", xlab="Chi Square Values", ylab="Density",
           main=paste("Chi Square Distribution, df=",df), axes=T,
           xlim=c(.9*xmin2,xmax2),
           cex.axis=1.5,cex.lab=1.5)
      j <- .x >=ub
      lines(.x, hx)
      segments(0,0,xmax2,0)
      segments(0,0,0,max(hx))
      #abline(h=0)
      #abline(v=0)
      abline(v=quantile,col="red")
      polygon(c(ub,.x[j],xmax2), c(0,hx[j],0), col="lightgrey")
      result <- paste("prob(Chi Sq \u2265 ", cvupper, ") =",
                      pdisplay(prob,digits.scientific=4))
      mtext(result,3,cex=1.5) 
    }
  }
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

