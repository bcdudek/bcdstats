#' Graphical display for a normal distribution, given a specified quantile
#'
#' A function to draw a normal distribution and shade a region defined by the
#' quantile
#'
#' \code{pgnorm}, \code{pgnormz}, \code{qgnormz}, and \code{gnorm} will display
#' two-tailed shaded regions for normal distributions (and std normal)
#'
#' @param quantile The quantile value (RV value) to be evaluated.  Note that
#' quantile should be positive if a two-tailed result is requested.
#' @param mean Arithmetic Mean of the Distribution
#' @param sd Standard Deviation of the Distribution
#' @param tail Area(s) to be shaded on the graph.  If \code{"upper"}, the
#' function returns a graph with the upper tail area shaded and the upper tail
#' probability displayed.  If \code{"lower"}, the function returns a graph with
#' the lower tail area shaded and the lower tail probability displayed.  This
#' function will not display two-tailed shaded regions.
#'
#' @section Warning: Specifying extremely high or low quantiles may not yield
#' visible shaded areas if the regions are beyond the limits of the X axis
#' scale of the graph. \cr In addition, probabilities are rounded to five
#' decimal places and will display as zero if below .000005.  Use the
#' \code{pnorm} function in this case.
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @examples
#' #shows upper tail by default
#' pgnorm(2500,mean=2000,sd=400)
#' pgnorm(115,mean=100,sd=15,tail="upper")
#' # note that 75.25 is 1.65 sd below the mean)
#' pgnorm(75.25,mean=100,sd=15,tail="lower")
#' pgnorm(130,mean=100,sd=15,tail="lower")
#' # note that pgnormz also yields the std normal without
#' # mean and sd arguments required
#' pgnorm(1.96,mean=0,sd=1,tail="upper")
#'
#' @export pgnorm
pgnorm <-
function(quantile,mean=0,sd=1, tail="upper")
{
xmin <--4
xmax <- 4
x <- seq(xmin,xmax,length=5000)*sd + mean
hx <- dnorm(x,mean,sd)

if(tail=="upper")
{
ub=quantile
prob <- round(pnorm(quantile,mean,sd,lower.tail=F),5)
cvupper=round(ub,5)
plot(x, hx, type="n", xlab="RV Values (X)", ylab="Density",
  #main="Standard Normal Distribution", axes=T)
  main = paste("Normal Distribution with mu=",mean,", sigma=",sd ),axes=T)
j <- x >=ub
lines(x, hx)
abline(h=0)
abline(v=mean)
abline(v=ub,col="red")
polygon(c(ub,x[j],xmax), c(0,hx[j],0), col="steelblue1")

result <- paste("P(X >=",cvupper,") =",
   prob)
mtext(result,3)
 }

else if(tail=="lower")
{
lb=quantile
cvlower=round(lb,5)
prob <- round(pnorm(quantile,mean,sd,lower.tail=T),5)
plot(x, hx, type="n", xlab="RV Values (X)", ylab="Density",
  # main="Standard Normal Distribution", axes=T)
  main = paste("Normal Distribution with mu=",mean,", sigma=",sd ),axes=T)
i <- x <= lb
lines(x, hx)
abline(h=0)
abline(v=mean)
abline(v=lb,col="red")
polygon(c(xmin,x[i],lb), c(0,hx[i],0), col="steelblue1")

result <- paste("P(X <=",cvlower,") =",
   prob)
mtext(result,3)
 }
}
