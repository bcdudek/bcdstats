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

if(FALSE) {
  pdisplay(1)
  pdisplay(0)
  pdisplay(1 * 10^-18) # Should say virtually zero
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
