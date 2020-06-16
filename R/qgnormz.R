#' Graphical display for a Standard Normal distribution, given a specified tail
#' probability
#'
#' A function to draw a Standard Normal distribution and shade the region(s)
#' defined by the probability argument
#'
#'
#' @param prob \code{prob} is the tail probability value to define the region
#' to be shaded on the graph and the corresponding quantile to be displayed.
#' note that if \code{tail="two"}, this probability is halved for display in
#' each tail.
#' @param tail Area(s) to be shaded on the graph.  If \code{"upper"}, the
#' function returns a graph with the upper tail area shaded and the upper tail
#' probability displayed.  If \code{"lower"}, the function returns a graph with
#' the lower tail area shaded and the lower tail probability displayed.  If
#' \code{"two"} the function returns a graph with both tails shaded and the
#' specified probability is halved between these two tails. tail=\code{"upper"}
#' is the default.
#'
#' @note This function can be very helpful in instructional situations and can
#' replace the "Z table" from textbooks.
#'
#' @section Warning: Specifying extremely high or low probabilities may not
#' yield visible shaded areas if the regions are beyond the limits of the X
#' axis scale of the graph.
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @examples
#'
#' qgnormz(.35) #shows upper tail by default
#' qgnormz(.16,tail="upper")
#' qgnormz(.16,tail="lower")
#' qgnormz(.975,tail="lower")
#' qgnormz(.025,tail="upper")
#'
#' @export qgnormz
qgnormz <-
function(prob, tail="upper")
{
mean=0
sd=1
xmin <--4
xmax <- 4
x <- seq(xmin,xmax,length=5000)*sd + mean
hx <- dnorm(x,mean,sd)

if(tail=="two")
{
lb=qnorm(prob/2,mean,sd,lower.tail=T)
ub=qnorm(prob/2,mean,sd,lower.tail=F)
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
result <- paste("P(",cvupper,"<= Z or  Z <=",cvlower,") =",
   prob)
mtext(result,3)
 }

else if(tail=="upper")
{
ub=qnorm(prob,mean,sd,lower.tail=F)
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

result <- paste("P(Z >=",cvupper,") =",
   prob)
mtext(result,3)
 }

else if(tail=="lower")
{
lb=qnorm(prob,mean,sd,lower.tail=T)
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

result <- paste("P(Z <=",cvlower,") =",
   prob)
mtext(result,3)
 }
}
