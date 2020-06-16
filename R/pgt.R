#' Graphical display for the "Student's" t distribution, given a specified
#' quantile
#'
#' A function to draw a Student's t distribution and shade a region defined by
#' the quantile
#'
#' @param quantile The quantile value (t value) to be evaluated.  Note that
#' quantile should be positive if a two-tailed result is requested.
#' @param df Degrees of freedom for the t distribution.  Note that the default
#' is 100.
#' @param tail Area(s) to be shaded on the graph.  If \code{"upper"}, the
#' function returns a graph with the upper tail area shaded and the upper tail
#' probability displayed.  If \code{"lower"}, the function returns a graph with
#' the lower tail area shaded and the lower tail probability displayed.  If
#' \code{"two"} the function returns a graph with both tails shaded and a
#' summed two-tailed probability displayed.
#'
#' @note This function can be very helpful in instructional situations and can
#' replace the "t table" from textbooks.
#'
#' @section Warning: Specifying extremely high or low quantiles may not yield
#' visible shaded areas if the regions are beyond the limits of the X axis
#' scale of the graph. \cr In addition, probabilities are rounded to five
#' decimal places and will display as zero if below .000005.  Use the \code{pt}
#' function in this case.
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @examples
#' pgt(-1.76, df=25, tail="lower")
#' pgt(1.76, df=25, tail="upper")
#' pgt(1.76, df=25, tail="two")
#' pgt(2.31, df=10, tail="lower")
#' # don't use this next example form with tail="two"
#' # instead use the following example example form
#' pgt(-1.7, df=10, tail="two")
#' pgt(1.7, df=10, tail="two")
#'
#' @export pgt
#' @importFrom stats qt
#' @importFrom stats pt
#' @importFrom stats dt
pgt <-
function(quantile,df=100,tail="upper")
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
ub=quantile
cvupper=round(ub,5)
prob <- round(pt(quantile,df=df,lower.tail=F),5)
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
#ub=quantile
lb=quantile
#cvupper=round(ub,5)
cvlower=round(lb,5)
prob <- round(pt(quantile,df=df,lower.tail=T),5)
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
ub=quantile
lb=-quantile
cvupper=round(ub,5)
cvlower=round(lb,5)
prob <- round((pt(ub,df=df,lower.tail=F))+(pt(lb,df=df,lower.tail=T)),5)
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
