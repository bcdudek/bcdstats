#' Graphical Depiction of the Negative Binomial Distribution
#'
#' \code{gbinom} permits specification of regions in a negative binomial
#' distribution
#'
#' The function plots probabilities of each of several possible numbers of
#' failures (ranging up to k) before the rth success.  The graph can be
#' confusing without exploring the examples below (as well as class materials
#' on the function).
#'
#' @param k this argument is used to set the x axis scale of the graph.  Often
#' choosing a value of 10 or 15 is adequate for textbook types of
#' demonstrations.  See the examples below for better illustration.
#' @param r The number of successes
#' @param p probability of a success on each trial.
#'
#' @note This function is a modification of the code for the Negative Binomial
#' Distribution found in the \code{Rcmdr} package.
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @seealso \code{\link[bcdstats:gbinom]{gbinom}}, for an analogous function
#' displaying the binomial distribution.
#'
#' @examples
#'
#' #setting \code{r} to 1 reduces the negative binomial (Pascal) distribution
#' #to a geometric distribution and the plot can be easily interpreted.
#' gnbinom(5,1,.5)
#' # probabilities of 0,1,2,3 or 4 failures before the second success.
#' gnbinom(4,2,.5)
#' # probabilities of 0 through 8 failures before the second success.
#' gnbinom(8,2,.5)
#' # probabilities of 0 through 10 failures before the third success.
#' gnbinom(10,3,.5)
#' # any value of p can be used
#' gnbinom(10,2,.1)
#'
#' @export gnbinom
#' @importFrom stats dnbinom
#' @importFrom graphics points
gnbinom <-
function(k,r,p)
{x <- 0:k
 plot(x, dnbinom(x, size=r, prob=p),
      xlab="Number of Failures Until Target Successes", ylab="Probability",
      main=paste("Negative Binomial (Pascal) Distribution:\nTarget successes = ", r , "  p = ", p),
      type="h",cex=2.5, col="steelblue3")
 points(x, dnbinom(x, size=r, prob=p), cex=1.5, col="steelblue3", pch=16)
 abline(h=0, col="gray")
 dnbinom(x,size=r,prob=p)
}
