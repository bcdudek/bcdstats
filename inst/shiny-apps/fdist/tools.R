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
# written by Jason Bryer  sep 2014
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

# functions for plotting F distributions

pgf <- function (quantile, df1 = 1, df2 = 5, tail = "upper") 
{
  xmin <- qf(5e-04, df1 = df1, df2 = df2)
  xmax <- qf(0.9995, df1 = df1, df2 = df2)
  .x <- seq(xmin, xmax, length = 10000)
  hx <- df(.x, df1, df2)
  ub <- quantile
  if (ub <= (df2/(df2 - 2))) {
    rightedge <- qf(0.99, df1 = df1, df2 = df2)
  }
  else if (ub > 1) {
    rightedge <- ub * (2.15^1.05)
  }
  if (df1 <= 6) {
    ymax = 0.6
  }
  else if (df1 > 6) {
    ymax = (max(hx) + 0.05)
  }
  prob <- (pf(quantile, df1, df2, lower.tail = F))
  par(cex.main=1.4,cex.axis=1.5,font.lab=2, mar = c(5, 6, 4, 5) + 0.1, 
      mgp = c(3.8, 0.7, 0),las=1,bty="n")
  plot(.x, hx, type = "n", xlab = "F Values", ylab = "Density", 
       main=paste("F Distribution, numerator df =",df1, ", denominator df =",df2), 
       axes = T, xlim = c(xmin, rightedge), ylim = c(0, ymax),
       cex.axis=1.5,cex.lab=1.5)
  j <- .x >= ub
  lines(.x, hx)
  segments(0,0,xmax,0)
  segments(0,0,0,max(hx))
  #abline(h = 0)
  #abline(v = 0)
  abline(v = ub, col = "red")
  polygon(c(ub, .x[j], xmax), c(0, hx[j], 0), col = "lightgrey")
  result <- paste("prob(F \u2265 ", ub, " ) =", 
                  pdisplay(prob,digits.scientific=4))
  mtext(result, 3,cex=1.3)
}

qgf <-
  function(prob,df1=1,df2=5,tail="upper")
  {
    xmin <- qf(.0005, df1=df1,df2=df2)
    xmax <- qf(.9995, df1=df1,df2=df2)
    .x <- seq(xmin,xmax,length=10000)
    hx <- df(.x,df1,df2)
    ub<-qf(prob,df1=df1,df2=df2,lower.tail=F)
    cvupper<-round(ub,5)
    
    if(ub <= (df2/(df2-2)))
    {
      rightedge <- qf(.99, df1=df1,df2=df2)
    }
    
    else if(ub > 1)
    {
      rightedge <- ub*(2.15**1.05)
    }
    
    if(df1<=6)
    {
      ymax=.6
    }
    else if(df1>6)
    {
      ymax=(max(hx)+.05)
    }
    par(cex.main=1.4,cex.axis=1.5,font.lab=2, mar = c(5, 6, 4, 5) + 0.1, 
        mgp = c(3.8, 0.7, 0),las=1,bty="n")
    plot(.x, hx, type="n", xlab="F Values", ylab="Density",
         main=paste("F Distribution, numerator df =",df1, ", denominator df =",df2),
         axes=T,
         xlim=c(xmin,rightedge),ylim=c(0,ymax),
         cex.axis=1.5,cex.lab=1.5)
    j <- .x >=ub
    lines(.x, hx)
    segments(0,0,xmax,0)
    segments(0,0,0,max(hx))
    #abline(h=0)
    #abline(v=0)
    abline(v=cvupper,col="red")
    polygon(c(ub,.x[j],xmax), c(0,hx[j],0), col="lightgrey")
    result <- paste("prob( F \u2265 ",cvupper,") =",
                    pdisplay(prob,digits.scientific=4))
    mtext(result,3,cex=1.3) 
    # qgf was modeled, in part on the function to draw
    # an F distribution found in the Rcmdr package
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
