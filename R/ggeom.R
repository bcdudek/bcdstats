#' Graph of Probabilities of Geometric Distributions.
#'
#' Graph the probabilities of the first success on the rth trial where the
#' elementary events are sampled from a stationary bernoulli process.
#'
#' The graph plots probabilities of 0:k failures before the first success.
#'
#' @param k The number of trials to display on the X axis.
#' @param p Probability of a success
#'
#' @note This function is a modification of code from the Rcmdr package.
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @examples
#'
#' ggeom(2,.5)
#' ggeom(10,.5)
#' ggeom(10,.2)
#' ggeom(15,.7)
#'
#' @export ggeom
#' @importFrom stats dgeom
#' @importFrom graphics points
ggeom <-
function(k,p)
     {x <- 0:k
      plot(x, dgeom(x, prob=p), xlab="Number of Failures Before First Succcess",
           ylab="Probability",
           main = paste("Geometric Distribution: p = ",  p),
           type="h", col="red")
      points(x, dgeom(x, prob=p), pch=16,cex=1.5,col="red")
      abline(h=0, col="gray")
      dgeom(x, prob=p)
}
