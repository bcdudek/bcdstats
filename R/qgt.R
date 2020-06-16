#' Graphical display for the "Student's" t distribution, given a specified
#' probability
#'
#' A function to produce a graph of a "Student's" t distribution with regions
#' shaded according to a specified probability.
#'
#' @param prob The tail probability value to define the region to be shaded on
#' the graph and the corresponding quantile to be displayed.
#' @param df Degrees of freedom for the \emph{t} distribution.  Note that the
#' default is 100.
#' @param tail Area(s) to be shaded on the graph.  If \code{"upper"}, the
#' function returns a graph with the upper tail area shaded and the upper tail
#' probability displayed.  If \code{"lower"}, the function returns a graph with
#' the lower tail area shaded and the lower tail probability displayed.  If
#' \code{"two"} the function returns a graph with both tails shaded and the
#' specified probability is halved between these two tails.
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
#' qgt(.05,df=25,tail="upper")
#' qgt(.01, df=12,tail="lower")
#' qgt(.90, df=15, tail="lower")
#' qgt(.01,df=50, tail="two")
#'
#' @export qgt
#' @importFrom stats qf
#' @importFrom stats qt
#' @importFrom stats dt
#' @importFrom stats pt
qgt <-
function(prob,df=1,tail="upper")
{
if(df==1)
{
xlim <- c(-65,65)
}
else if(df==2)
{
xlim <- c(-15,15)
}
else if(df==3)
{
xlim <- c(-10,10)
}
else if(df==4)
{
xlim <- c(-8,8)
}
else if(df==5)
{
xlim <- c(-6,6)
}
else if(df==6)
{
xlim <- c(-5,5)
}
else if(df==7)
{
xlim <- c(-5,5)
}
else if(df==8)
{
xlim <- c(-4.8,4.8)
}
else if(df==9)
{
xlim <- c(-4.6,4.6)
}
else if(df==10)
{
xlim <- c(-4.5,4.5)
}
else if(df>=10)
{
xlim <- c(-4,4)
}
if(tail=="upper")
{
xmin <- qt(.0005, df=df)
xmax <- qt(.9995, df=df)
.x <- seq(xmin,xmax,length=10000)
hx <- dt(.x,df)
ub=qt(prob,df,lower.tail=F)
cvupper=round(ub,5)
plot(.x, hx, type="n", xlab="t Values", ylab="Density",
    main=paste("t Distribution, df=",df), axes=T,
	xlim=xlim)
j <- .x >=ub
lines(.x, hx)
abline(h=0)
abline(v=0)
abline(v=cvupper,col="red")
polygon(c(ub,.x[j],xmax), c(0,hx[j],0), col="steelblue1")
result <- paste("P(",cvupper,">= t ) =",
   prob)
mtext(result,3)
 }
else if(tail=="lower")
{
xmin <- qt(.0005, df=df)
xmax <- qt(.9995, df=df)
.x <- seq(xmin,xmax,length=10000)
hx <- dt(.x,df)
ub=qt(prob,df,lower.tail=F)
lb=qt(prob,df,lower.tail=T)
cvupper=round(ub,5)
cvlower=round(lb,5)
plot(.x, hx, type="n", xlab="t Values", ylab="Density",
    main=paste("t Distribution, df=",df), axes=T,
	xlim=xlim)
i <- .x <= lb
lines(.x, hx)
abline(h=0)
abline(v=0)
abline(v=cvlower,col="red")
polygon(c(xmin,.x[i],lb), c(0,hx[i],0), col="steelblue1")
result <- paste("P(",cvlower,"<= t ) =",
   prob)
mtext(result,3)
}

else if(tail=="two")
{
xmin <- qt(.0005, df=df)
xmax <- qt(.9995, df=df)
.x <- seq(xmin,xmax,length=10000)
hx <- dt(.x,df)
ub=qt(prob/2,df,lower.tail=F)
lb=qt(prob/2,df,lower.tail=T)
cvupper=round(ub,5)
cvlower=round(lb,5)
plot(.x, hx, type="n", xlab="t Values", ylab="Density",
    main=paste("t Distribution, df=",df), axes=T,
	xlim=xlim)
i <- .x <= lb
j <- .x >=ub
lines(.x, hx)
abline(h=0)
abline(v=0)
abline(v=cvupper,col="red")
abline(v=cvlower,col="red")
polygon(c(xmin,.x[i],lb), c(0,hx[i],0), col="steelblue1")
polygon(c(ub,.x[j],xmax), c(0,hx[j],0), col="steelblue1")
area <- pt(ub,df=df) - pt(lb,df=df)
result <- paste("P(",cvlower,"<= t or  t >=",cvupper,") =",
   prob)
mtext(result,3)
}

}
