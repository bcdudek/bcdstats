# graphing function to plot figures of binomial distribution


gbinom2 <-
  function(n, p, low=0, high=n,scale = T, a=NA,b=NA,calcProb=!all(is.na(c(a,b))),quantile=NA,calcQuant=!is.na(quantile))
  {
    sd = sqrt(n * p * (1 - p))
    if(scale && (n > 10)) {
      low = max(0, round(n * p - 4 * sd))
      high = min(n, round(n * p + 4 * sd))
    }
    values = low:high
    probs = dbinom(values, n, p)
    par(cex.main=1.5,
        cex.axis=1.4,
        cex.lab=1.4, 
        mar = c(6, 7, 5, 5) + 0.1,
        font.lab=2,
        mgp = c(3.5, 1, 0),
        las=1,bty="n")
    plot(c(low,high), c(0,max(probs)), type = "n",
         xlab = "Number of Successes",
         ylab = "Probability",
         main = paste("Binomial Distribution \n", "n =", n, ", p =", p))
    lines(values, probs, type = "h", cex=1.75,lwd=2,
          #col = "darkblue")
          col = "steelblue4")
    abline(h=0,col="grey70")
    if(calcProb) {
      if(is.na(a))
        a = 0
      if(is.na(b))
        b = n
      if(a > b) {
        d = a
        a = b
        b = d
      }
      a = round(a)
      b = round(b)
      prob = pbinom(b,n,p) - pbinom(a-1,n,p)
#      title(paste("P(",a," \u2264 X \u2264 ",b,") = ",round(prob,6),sep=""),line=0,
      title(paste("P(",a," \u2264 X \u2264 ",b,") = ",pdisplay(prob,digits.scientific=7),sep=""),line=0,
                  
                        col.main="gray30")
      u = seq(max(c(a,low)),min(c(b,high)),by=1)
      v = dbinom(u,n,p)
      lines(u,v,type="h", cex=1.75,lwd=3,
            col="darkred")
            #col="steelblue4")
    }
    else if(calcQuant==T) {
      if(quantile < 0 || quantile > 1)
        stop("quantile must be between 0 and 1")
      x = qbinom(quantile,n,p)
      title(paste("The ",quantile," quantile = ",x,sep=""),line=0,
            col.main="gray30")
      u = 0:x
      v = dbinom(u,n,p)
      lines(u,v,type="h", cex=1.75,lwd=3,
            col="darkred")
      #col="steelblue4")
      #lines(u,v,type="h", cex=1.75,col="steelblue4",lwd=4)
    }
    return(invisible())
  }

############################################################################
# The renderRmd function is used for markup in the "about" tab.  
# It was adapted from an application by Vincent Nijs, UCSD.
# https://github.com/mostly-harmless/radyant/tree/master/inst/marketing
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
pdisplay <- function(x, threshold=7, 
                     digits=threshold, 
                     nsmall=digits,
                     digits.scientific=nsmall,
                     nsmall.scientific=(digits.scientific-1)) {
  if(digits < threshold) { 
    warning('Setting digits less than threshold may yield unexpected results')
  }
  results <- sapply(x, function(x1) {
    result <- ''
    if(x1 > 0 & x1 <= 1 / (10 ^ threshold)) {
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

