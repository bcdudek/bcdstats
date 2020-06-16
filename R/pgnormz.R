#' Graphical display for a Standard Normal distribution, given a specified
#' quantile ("Z") value
#'
#' A function to draw a Standard Normal distribution and shade a region defined
#' by the quantile argument
#'
#'
#' @param quantile The quantile value ("Z" value) to be evaluated.  Note that
#' quantile should be positive if a two-tailed result is requested.
#' @param tail Area(s) to be shaded on the graph.  If \code{"upper"}, the
#' function returns a graph with the upper tail area shaded and the upper tail
#' probability displayed.  If \code{"lower"}, the function returns a graph with
#' the lower tail area shaded and the lower tail probability displayed.  If
#' \code{"two"} the function returns a graph with both tails shaded and a
#' summed two-tailed probability displayed.
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
#' pgnormz(1.2)
#' pgnormz(1.0,tail="upper")
#' pgnormz(-1.64,tail="lower")
#' pgnormz(2.0,tail="lower")
#' pgnormz(1.96,tail="upper")
#'
#' @export pgnormz
pgnormz <-
function(quantile, tail="upper")
{
mean=0
sd=1
xmin <--4
xmax <- 4
x <- seq(xmin,xmax,length=5000)*sd + mean
hx <- dnorm(x,mean,sd)

if(tail=="two")
{
lb=-quantile
ub=quantile
cvupper=round(ub,5)
cvlower=round(lb,5)
plot(x, hx, type="n", xlab="Z Values", ylab="Density",
    main="Standard Normal Distribution", axes=T)
#   main = paste("Normal Distribution with mu=",mean,", sigma=",sd ))

i <- x <= lb
j <- x >=ub
lines(x, hx)
abline(h=0)
abline(v=mean)
abline(v=lb,col="red")
abline(v=ub,col="red")
polygon(c(xmin,x[i],lb), c(0,hx[i],0), col="steelblue1")
polygon(c(ub,x[j],xmax), c(0,hx[j],0), col="steelblue1")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
prob <- round(1-area,5)
result <- paste("P(",cvupper,"<= Z or  Z <=",cvlower,") =",
   prob)
mtext(result,3)
 }

else if(tail=="upper")
{
ub=quantile
cvupper=round(ub,5)
plot(x, hx, type="n", xlab="Z Values", ylab="Density",
  main="Standard Normal Distribution", axes=T)
#  main = paste("Normal Distribution with mu=",mean,", sigma=",sd ))
j <- x >=ub
lines(x, hx)
abline(h=0)
abline(v=mean)
abline(v=ub,col="red")
polygon(c(ub,x[j],xmax), c(0,hx[j],0), col="steelblue1")
prob <- round(pnorm(quantile,lower.tail=F),5)
result <- paste("P(Z >=",cvupper,") =",
   prob)
mtext(result,3)
 }

else if(tail=="lower")
{
lb=quantile
cvlower=round(lb,5)
plot(x, hx, type="n", xlab="Z Values", ylab="Density",
   main="Standard Normal Distribution", axes=T)
#  main = paste("Normal Distribution with mu=",mean,", sigma=",sd ))
i <- x <= lb
lines(x, hx)
abline(h=0)
abline(v=mean)
abline(v=lb,col="red")
polygon(c(xmin,x[i],lb), c(0,hx[i],0), col="steelblue1")
prob <- round(pnorm(quantile,lower.tail=T),5)
result <- paste("P(Z <=",cvlower,") =",
   prob)
mtext(result,3)
 }
}
