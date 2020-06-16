#' Graph of the Binomial Distribution with point and region probabilities
#' illustrated.
#'
#' Binomial Distributions of an N and p values can be plotted.  Scale limits
#' can be specified, as well as regions of the RV scale for determining
#' probabilities.  These regions are indicated with color on the graph and
#' probabilities are also displayed.
#'
#'
#' @param n Number of trials.
#' @param p Probability of a Success.
#' @param low Lower limit of the X axis scale.  Set to zero by default.
#' @param high Upper limit of the X axis scale.  Set to N, the number of
#' trials, by default.
#' @param scale Truncates the scale when TRUE.  Shows full scale when FALSE
#' @param a Arguments \code{a} and/or \code{b} can be used to set regions of
#' the RV scale for which to color code and compute probabilities.  See the
#' examples below for several illustrations.  The \code{a} value would
#' typically be the smaller value if both \code{a} and \code{b} are used.
#' @param b Arguments \code{a} and/or \code{b} can be used to set regions of
#' the RV scale for which to color code and compute probabilities.  See the
#' examples below for several illustrations.  The \code{a} value would
#' typically be the smaller value if both \code{a} and \code{b} are used.
#' @param findquantile Takes a cumulative left tail probability for
#' which a quantile is returned. Default is NA.  Arguments \code{a}
#' and \code{b} override \code{findquantile}
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @references This function is a modification of a function originally coded
#' by Brett Larget, Univ. Wisconsin.\cr
#' http://www.stat.wisc.edu/~larget/R/prob.R
#'
#' @examples
#'
#' gbinom(4,.5)
#' gbinom(100,.5)
#' gbinom(4,.4)
#' gbinom(10,.4)
#' gbinom(25,.7)
#' gbinom(4,.5,a=3)
#' gbinom(4,.5,b=1)
#' gbinom(4,.5,a=3,b=1)
#' gbinom(100,.667,a=60,b=70)
#' # improve the previous graph by resetting the x axis limits
#' gbinom(100,.667,a=60,b=70,low=45,high=85)
#'
#' @export gbinom
#' @importFrom stats sd
#' @importFrom stats dbinom
#' @importFrom stats pbinom
#' @importFrom graphics title
#' @importFrom stats qbinom
gbinom <-
function(n, p, low=0, high=n,scale = F, a=NA,b=NA,
         findquantile=NA)
{
  calcProb=!all(is.na(c(a,b)))
  calcQuant=!is.na(findquantile)
  sd = sqrt(n * p * (1 - p))
  if(scale && (n > 10)) {
	low = max(0, round(n * p - 4 * sd))
	high = min(n, round(n * p + 4 * sd))
  }
  values = low:high
  probs = dbinom(values, n, p)
  plot(c(low,high), c(0,max(probs)), type = "n", xlab = "Possible Values",
      ylab = "Probability",
      main = paste("Binomial Distribution \n", "n =", n, ", p =", p))
  lines(values, probs, type = "h", cex=1.75, col = "firebrick3")
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
    title(paste("P(",a," <= X <= ",b,") = ",round(prob,6),sep=""),line=0,col.main="steelblue4")
    u = seq(max(c(a,low)),min(c(b,high)),by=1)
    v = dbinom(u,n,p)
    lines(u,v,type="h", cex=1.75,col="steelblue4")
  }
  else if(calcQuant==T) {
    if(findquantile < 0 || findquantile > 1)
      stop("findquantile must be between 0 and 1")
    x = qbinom(findquantile,n,p)
    title(paste("The ",findquantile," quantile = ",x,sep=""),line=0,col.main="steelblue4")
    u = 0:x
    v = dbinom(u,n,p)
    lines(u,v,type="h", cex=1.75,col="steelblue4")
  }
  return(invisible())
}
