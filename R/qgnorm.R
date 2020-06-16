#' Graphical display for a normal distribution, given a specified tail
#' probability
#'
#' A function to draw a normal distribution and shade the region(s) defined by
#' the probability argument
#'
#'
#' @param prob The tail probability value to define the region to be shaded on
#' the graph and the corresponding quantile to be displayed.  note that if
#' \code{tail="two"}, this probability is halved for display in each tail.
#' @param mean \code{mean} is the Distribution Mean
#' @param sd \code{sd} is the Distribution Standard Deviation
#' @param tail Area(s) to be shaded on the graph.  If \code{"upper"}, the
#' function returns a graph with the upper tail area shaded and the upper tail
#' probability displayed.  If \code{"lower"}, the function returns a graph with
#' the lower tail area shaded and the lower tail probability displayed.  If
#' \code{"two"} the function returns a graph with both tails shaded and the
#' specified probability is halved between these two tails. tail=\code{"upper"}
#' is the default.
#'
#' @note This function can be very helpful in instructional situations and can
#' replace the "t table" from textbooks.
#'
#' @section Warning: Specifying extremely high or low probabilities may not
#' yield visible shaded areas if the regions are beyond the limits of the X
#' axis scale of the graph.
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @examples
#'
#' qgnorm(.35,mean=2000,sd=400) #shows upper tail by default
#' qgnorm(.16,mean=100,sd=15,tail="upper")
#' qgnorm(.16,mean=100,sd=15,tail="lower")
#' qgnorm(.975,mean=100,sd=15,tail="lower")
#' qgnorm(.025,mean=0,sd=1,tail="upper")
#' #note that pgnormz also yields the std normal
#' #without mean and sd arguments required#'
#' @export qgnorm
qgnorm <-
function(prob,mean=0,sd=1, tail="upper")
{
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
plot(x, hx, type="n", xlab="RV Values (X)", ylab="Density",
#  main="Standard Normal Distribution", axes=T)
main = paste("Normal Distribution with mu=",mean,", sigma=",sd ),axes=T)

i <- x <= lb
j <- x >=ub
lines(x, hx)
abline(h=0)
abline(v=mean)
abline(v=ub,col="red")
abline(v=lb,col="red")
polygon(c(xmin,x[i],lb), c(0,hx[i],0), col="steelblue1")
polygon(c(ub,x[j],xmax), c(0,hx[j],0), col="steelblue1")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",cvupper,"<= X or  X <=",cvlower,") =",
   prob)
mtext(result,3)
 }

else if(tail=="upper")
{
ub=qnorm(prob,mean,sd,lower.tail=F)
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
lb=qnorm(prob,mean,sd,lower.tail=T)
cvlower=round(lb,5)
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
