#' Graphing a Normal Distribution (from Brett Larget)
#'
#' \code{gnorm} plots a density function for a normal distribution of specified
#' mean and standard deviation.  It will also accept specification of one, or
#' two "cutpoints" and shade the corresponding tail regions or the region
#' between two cutpoints.  The function also displays the corresponding tail or
#' region probability.
#'
#'
#' @param mu \code{mu} is the Normal Distribution mean. Required.  Use 0 for
#' std normal.
#' @param sigma \code{sigma} is the Normal Distribution Std Deviation.
#' Required.  Use 1.0 for std normal.
#' @param a optional specification of a quantile value that can serve as a
#' "cutpoint".  \code{gnorm} will shade the tail region demarcated by this
#' value.  \code{a} can be used in conjunction with \code{b} to find
#' probabilities between the two values.
#' @param b optional specification of a quantile value that can serve as a
#' "cutpoint".  \code{gnorm} will shade the tail region demarcated by this
#' value.  \code{b} can be used in conjunction with \code{a} to find
#' probabilities between the two values.
#' @param findquantile Takes a cumulative left tail probability for
#' which a quantile is returned. Default is NA.  Arguments \code{a}
#' and \code{b} override \code{findquantile}
#'
#' @note This function can be very helpful in an instructional setting.

#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @seealso \code{\link[bcdstats:pgnorm]{pgnorm}},
#' \code{\link[bcdstats:pgnormz]{pgnormz}},
#' \code{\link[bcdstats:qgnorm]{qgnorm}},
#' \code{\link[bcdstats:qgnormz]{qgnormz}}
#' @references This function is a modification of a function coded by Brett
#' Larget, Univ Wisconsin.\cr http://www.stat.wisc.edu/~larget/R/prob.R
#' @examples
#'
#' gnorm(0,1)
#' gnorm(0,1,a=1.65)
#' gnorm(0,1,a=-1.65)
#' gnorm(0,1,b=1.65)
#' gnorm(0,1,a=1.65,b=1.65)
#' gnorm(0,1,a=1.96,b=-1.65)
#' gnorm(100,15,a=130)
#' gnorm(100,15,a=85)
#' gnorm(100,15,a=85,b=115)
#' gnorm(800,20,a=837,b=763)
#'
#' @export gnorm
#' @importFrom graphics axis
#' @importFrom graphics text
#' @importFrom stats pnorm
#' @importFrom stats qnorm
gnorm <-
function(mu, sigma,a=NA,b=NA,findquantile=NA)
{
  calcProb=!all(is.na(c(a,b)))
  calcQuant=!is.na(findquantile)
  values = seq(-1,1,.005) * 4 * sigma + mu
  probs = dnorm(values, mu, sigma)
  plot(values, probs, axes = F, type = "n", xlab = "Possible Values",
    ylab = "Probability Density",
    main = substitute(paste("Normal Distribution with ",mu == m,", ",sigma == s),list(m=mu,s=sigma)))
  axis(1, pos = 0)
  abline(0,0,col=1)
  abline(v=mu)
  lines(values, probs, col = "tomato4")
  lo = mu - 4 * sigma
  hi = mu + 4 * sigma
  h = dnorm(mu,mu,sigma)
  cex=0.8
  if(calcProb==T)
  {
    if(!is.na(a) && !is.na(b) && a > b){
      d = a; a = b; b = d
    }
    if(is.na(a) || a <= lo){ ulo = lo }
    else if(a <= hi){ ulo = a }
    else { ulo = hi }
    if(is.na(b) || b >= hi){ uhi = hi }
    else if(b >= lo){ uhi = b }
    else { uhi = lo }
    u = seq(ulo,uhi,length=601)
    lines(u,dnorm(u,mu,sigma),type="h",col="tomato4")
    if(!is.na(a) && !is.na(b)){
      text(mu - 3.9 * sigma, 0.8 * h,
        paste("P( ",a," < X < ",b," ) = ",
  	round(pnorm(b,mu,sigma)-pnorm(a,mu,sigma),digits=4),sep=""),
        adj=0,col="tomato4",cex=cex)
      text(mu - 3.9 * sigma, 0.6 * h,
        paste("P( X < ",a," ) = ",
          round(pnorm(a,mu,sigma),digits=4),sep=""),adj=0,col="blue4",cex=cex)
      text(mu + 3.9 * sigma, 0.5 * h,
        paste("P( X > ",b," ) = ",
       	round(1-pnorm(b,mu,sigma),digits=4),sep=""),adj=1,col="blue4",cex=cex)
    }
    else if(!is.na(a) && is.na(b)){
      text(mu - 3.9 * sigma, 0.6 * h,
        paste("P( X < ",a," ) = ",
          round(pnorm(a,mu,sigma),digits=4),sep=""),adj=0,col="blue4",cex=cex)
      text(mu + 3.9 * sigma, 0.5 * h,
        paste("P( X > ",a," ) = ",
       	round(1-pnorm(a,mu,sigma),digits=4),sep=""),adj=1,col="blue4",cex=cex)
    }
    else if(is.na(a) && !is.na(b)){
      text(mu - 3.9 * sigma, 0.6 * h,
        paste("P( X < ",b," ) = ",
          round(pnorm(b,mu,sigma),digits=4),sep=""),adj=0,col="blue4",cex=cex)
      text(mu + 3.9 * sigma, 0.5 * h,
        paste("P( X > ",b," ) = ",
       	round(1-pnorm(b,mu,sigma),digits=4),sep=""),adj=1,col="blue4",cex=cex)
    }
  }
  else if(calcQuant==T)
  {
    zoffset = -0.02
    if( findquantile <= 0 || findquantile >= 1) findquantile = 0.5
    x = qnorm(findquantile,mu,sigma)
    if( x > lo && x < hi)
    {
      u = seq(lo,x,length=601)
      lines(u,dnorm(u,mu,sigma),type="h",col="tomato4")
      text(x, zoffset * h,
  	paste("z = ",round(qnorm(findquantile),2),sep=""),adj=0.5,col="blue4",cex=cex)
    }
    else if(x >= hi)
    {
      u = seq(lo,hi,length=601)
      lines(u,dnorm(u,mu,sigma),type="h",col="tomato4")
      text(hi, zoffset * h,
  	paste("z = ",round(qnorm(findquantile),2),sep=""),adj=0.5,col="blue4",cex=cex)
    }
    else if( x <= lo)
    {
      text(lo, zoffset * h,
  	paste("z = ",round(qnorm(findquantile),2),sep=""),adj=0.5,col=4,cex=cex)
    }
    text(mu - 3.9 * sigma, 0.5 * h,
      paste("P( X < ",signif(x,4)," ) = ",
  	round(findquantile,digits=4),sep=""),adj=0,col="blue4",cex=cex)
  }
  return(invisible())
}
