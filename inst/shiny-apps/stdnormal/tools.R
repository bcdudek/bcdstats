# function to render markdown text for about panel and other things
# not used as of ver 1.6
#renderRmd <- function(path, input) {
#  return(renderText({
#    if (!require(knitr))
#      stop("knitr package is not installed")
#    if (!require(markdown))
#      stop("Markdown package is not installed")
#    # shiny:::dependsOnFile(path)
#    contents <-
#      paste(readLines(path, warn = FALSE), collapse = '\n')
#    myenvir <- new.env() # Perhaps use parent.frame() ?!
#    assign('input', input, envir = myenvir)
#    html <-
#      knitr::knit2html(text = contents, fragment.only = TRUE, envir = myenvir)
#    Encoding(html) <- 'UTF-8'
#    return(HTML(html))
#  }))
#}

# function for displaying pvalues with paste function on graphs
# written by Jason Bryer sep 2014
pdisplay <- function(x, threshold = 5,
                     digits = threshold,
                     nsmall = digits,
                     digits.scientific = nsmall,
                     nsmall.scientific = (digits.scientific - 1)) {
  if (digits < threshold) {
    warning('Setting digits less than threshold may yield unexpected results')
  }
  results <- sapply(x, function(x1) {
    result <- ''
    if (x1 <= 1 / (10 ^ threshold)) {
      while ((x1 * (10 ^ threshold)) < 1) {
        threshold <- threshold + 1
      }
      result <-
        paste0(
          format(
            x1 * (10 ^ threshold), digits = digits.scientific,
            drop0trailing = FALSE, nsmall = nsmall.scientific
          ),
          'e-', threshold
        )
    } else {
      result <- format(
        round(x1, digits = digits),
        digits = digits,
        drop0trailing = FALSE,
        scientific = FALSE,
        nsmall = (nsmall)
      )
    }
    return(result)
  })
  return(results)
}

# examples of pdisplay usage
#if (FALSE) {
#  pdisplay(2112.0)
#  pdisplay(.000002112)
#  pdisplay(.000002112112112)
#  pdisplay(.000000201)
#  
#  pdisplay(pnorm(3, lower.tail = FALSE))
##  pdisplay(pnorm(3.8, lower.tail = FALSE))
#  pdisplay(pnorm(4.75, lower.tail = FALSE))
#  pdisplay(pnorm(4.8, lower.tail = FALSE), digits.scientific = 4)
#}

# functions for plotting the std normal distribution

pgnormz <-
  function(quantile, tail = "upper")
  {
    mean = 0
    sd = 1
    xmin <- -4
    xmax <- 4
    x <- seq(xmin,xmax,length = 5000) * sd + mean
    hx <- dnorm(x,mean,sd)
    
    if (tail == "two")
    {
      lb = -quantile
      ub = quantile
      cvupper = round(ub,5)
      cvlower = round(lb,5)
      par(
        cex.main = 1.5,cex.axis = 1.5,font.lab = 2, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0),las =
          1,bty = "n"
      )
      plot(
        x, hx, type = "n", xlab = "Z Values", ylab = "Density",
        main = "Standard Normal Distribution", axes = T,cex.axis = 1.5,cex.lab =
          1.5,cex.main = 1.5
      )
      #   main = paste("Normal Distribution with mu=",mean,", sigma=",sd ))
      
      i <- x <= lb
      j <- x >= ub
      lines(x, hx)
      abline(h = 0)
      abline(v = mean)
      abline(v = lb,col = "red")
      abline(v = ub,col = "red")
      polygon(c(xmin,x[i],lb), c(0,hx[i],0), col = "lightgrey")
      polygon(c(ub,x[j],xmax), c(0,hx[j],0), col = "lightgrey")
      
      #area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
      #prob <- (1-area)
      plow <- pnorm(lb,mean,sd)
      phigh <- pnorm(ub,mean,sd,lower.tail = FALSE)
      prob <- plow + phigh
      result <-
        paste0(
          "prob(Z  \u2264 ",cvlower," or  Z \u2265 ",cvupper,") = ",
          pdisplay(prob,digits.scientific = 4)
        )
      mtext(result,3,cex = 1.3)
    }
    
    else if (tail == "upper")
    {
      ub = quantile
      cvupper = round(ub,5)
      par(
        cex.main = 1.5,cex.axis = 1.5,font.lab = 2, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0),las =
          1,bty = "n"
      )
      plot(
        x, hx, type = "n", xlab = "Z Values", ylab = "Density",
        main = "Standard Normal Distribution", axes = T,cex.axis = 1.5,cex.lab =
          1.5,cex.main = 1.5
      )
      #  main = paste("Normal Distribution with mu=",mean,", sigma=",sd ))
      j <- x >= ub
      lines(x, hx)
      abline(h = 0)
      abline(v = mean)
      abline(v = ub,col = "red")
      polygon(c(ub,x[j],xmax), c(0,hx[j],0), col = "lightgrey")
      prob <- pnorm(quantile,lower.tail = F)
      result <-
        paste0("prob(Z \u2265",cvupper,") =",pdisplay(prob,digits.scientific =
                                                        4))
      #result <- caption
      mtext(result,3,cex = 1.3)
    }
    
    else if (tail == "lower")
    {
      lb = quantile
      cvlower = round(lb,5)
      par(
        cex.main = 1.5,cex.axis = 1.5,font.lab = 2, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0),las =
          1,bty = "n"
      )
      plot(
        x, hx, type = "n", xlab = "Z Values", ylab = "Density",
        main = "Standard Normal Distribution", axes = T,cex.axis = 1.5,cex.lab =
          1.5,cex.main = 1.5
      )
      #  main = paste("Normal Distribution with mu=",mean,", sigma=",sd ))
      i <- x <= lb
      lines(x, hx)
      abline(h = 0)
      abline(v = mean)
      abline(v = lb,col = "red")
      polygon(c(xmin,x[i],lb), c(0,hx[i],0), col = "lightgrey")
      prob <- pnorm(quantile,lower.tail = T)
      result <- paste0("prob(Z \u2264 ",cvlower,") =",
                       pdisplay(prob,digits.scientific = 4))
      mtext(result,3,cex = 1.3)
    }
  }


qgnormz <-
  function(prob, tail = "upper")
  {
    mean = 0
    sd = 1
    xmin <- -4
    xmax <- 4
    x <- seq(xmin,xmax,length = 5000) * sd + mean
    hx <- dnorm(x,mean,sd)
    
    if (tail == "two")
    {
      lb = qnorm(prob / 2,mean,sd,lower.tail = T)
      ub = qnorm(prob / 2,mean,sd,lower.tail = F)
      cvupper = round(ub,5)
      cvlower = round(lb,5)
      par(
        cex.main = 1.5,cex.axis = 1.5,font.lab = 2, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0),las =
          1,bty = "n"
      )
      plot(
        x, hx, type = "n", xlab = "Z Values", ylab = "Density",
        main = "Standard Normal Distribution", axes = T,cex.axis = 1.5,cex.lab =
          1.5,cex.main = 1.5
      )
      #   main = paste("Normal Distribution with mu=",mean,", sigma=",sd ))
      
      i <- x <= lb
      j <- x >= ub
      lines(x, hx)
      abline(h = 0)
      abline(v = mean)
      abline(v = lb,col = "red")
      abline(v = ub,col = "red")
      polygon(c(xmin,x[i],lb), c(0,hx[i],0), col = "lightgrey")
      polygon(c(ub,x[j],xmax), c(0,hx[j],0), col = "lightgrey")
      
      area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
      result <-
        paste0(
          "prob(Z \u2264 ",cvlower," or  Z \u2265",cvupper,") =",
          pdisplay(prob,digits.scientific = 4)
        )
      mtext(result,3,cex = 1.3)
    }
    
    else if (tail == "upper")
    {
      ub = qnorm(prob,mean,sd,lower.tail = F)
      cvupper = round(ub,5)
      par(
        cex.main = 1.5,cex.axis = 1.5,font.lab = 2, mar = c(5, 6, 4, 5) + 0.1,mgp = c(3.5, 1, 0),las =
          1,bty = "n"
      )
      plot(
        x, hx, type = "n", xlab = "Z Values", ylab = "Density",
        main = "Standard Normal Distribution", axes = T,cex.axis = 1.5,cex.lab =
          1.5,cex.main = 1.5
      )
      #  main = paste("Normal Distribution with mu=",mean,", sigma=",sd ))
      j <- x >= ub
      lines(x, hx)
      abline(h = 0)
      abline(v = mean)
      abline(v = ub,col = "red")
      polygon(c(ub,x[j],xmax), c(0,hx[j],0), col = "lightgrey")
      
      result <- paste0("prob(Z \u2265",cvupper,") =",
                       pdisplay(prob,digits.scientific = 4))
      mtext(result,3,cex = 1.3)
    }
    
    else if (tail == "lower")
    {
      lb = qnorm(prob,mean,sd,lower.tail = T)
      cvlower = round(lb,5)
      par(
        cex.main = 1.5,cex.axis = 1.5,font.lab = 2, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0),las =
          1,bty = "n"
      )
      plot(
        x, hx, type = "n", xlab = "Z Values", ylab = "Density",
        main = "Standard Normal Distribution", axes = T,cex.axis = 1.5,cex.lab =
          1.5,cex.main = 1.5
      )
      #  main = paste("Normal Distribution with mu=",mean,", sigma=",sd ))
      i <- x <= lb
      lines(x, hx)
      abline(h = 0)
      abline(v = mean)
      abline(v = lb,col = "red")
      polygon(c(xmin,x[i],lb), c(0,hx[i],0), col = "lightgrey")
      
      result <- paste0("prob(Z \u2264",cvlower,") =",
                       pdisplay(prob,digits.scientific = 4))
      mtext(result,3,cex = 1.3)
    }
  }
