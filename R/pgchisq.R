#' Graphical display for the Chi Squared distribution, given a specified
#' quantile
#'
#' A function to produce a graph of a Chi Squared distribution with regions
#' shaded according to a specified quantile ( Chi Squared value).
#'

#'
#' @param quantile The Chi Squared value to be evaluated, (\emph{e.g., 3.84)}
#' @param df The \code{df} for the \emph{Chi Squared} Distribution.  Default is
#' 1 df (see note below)
#'
#' @note The scaling on plots of Chi Squared Distributions of varying degrees
#' of freedom requires considerable adjustment. When df=1, \code{pgchisq}
#' returns a two-paneled figure.  The plot on the left is a fairly full scale
#' plot of the distribution without shading of a tail region.  The plot on the
#' right has axes truncated so that scale expansion will permit visualization
#' of the relevant area under the curve.
#'
#' Note that the \code{pgchisq} function only displays upper tail
#' regions since most inference with the Chi Squared distribution
#' only requires upper tail evaluation.  The lower tail probability can be
#' found as 1 minus the returned upper tail probability.
#'
#' @section Warning: Specifying extremely high or low quantiles may not yield
#' visible shaded areas if the regions are beyond the limits of the X axis
#' scale of the graph.
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @seealso \code{\link[bcdstats:qgchisq]{qgchisq}}
#' @examples
#'
#' pgchisq(3.84,df=1)
#' pgchisq(4.20,df=2)
#' pgchisq(30.5,df=25)
#' pgchisq(200,df=250)
#' pgchisq(4.75,df=1)
#' pgchisq(4.75,df=2)
#' pgchisq(45,df=25)
#' pgchisq(475,df=250)
#'
#' @export pgchisq
#' @importFrom stats qchisq
#' @importFrom stats dchisq
#' @importFrom stats pchisq
#' @importFrom graphics layout
pgchisq <-
function(quantile,df=1)
{

if(df==1)
{
xmin <- qchisq(.0005, df=1, ncp=0)
xmax <- qchisq(.999, df=1, ncp=0)
.x <- seq(xmin,xmax,length=1000)
hx <- dchisq(.x,df=1)
ub=quantile
cvupper=round(ub,5)
prob <- round(pchisq(quantile,df=1,lower.tail=F),5)
#if(Sys.info()["sysname"]=="Windows")
#{
#win.graph(width=12,height=6)
#}
#else if(Sys.info()["sysname"]=="Darwin")
#{
#quartz(width=12,height=6)
#}
#else if(Sys.info()["sysname"]=="Linux")
#{
#x11(width=12,height=6)
#}
layout(matrix(c(1,2), nrow=1, ncol=2,byrow=TRUE))
plot(.x, hx, type="n", xlab="Chi Square Values", ylab="Density",
     ylim=c(0,6),
    main="Full Chi Square Distribution, df=1", axes=T,xlim=c(0,9))
j <- .x >=ub
lines(.x, hx)
abline(h=0)
abline(v=0)
plot(.x, hx, type="n", xlab="Chi Square Values", ylab="Density",
     ylim=c(0,.25),
    main="Chi Square (Axis expansion), df=1", axes=T,xlim=c(0,xmax))
j <- .x >=ub
lines(.x, hx)
abline(h=0)
abline(v=0)
abline(v=quantile,col="red")
polygon(c(ub,.x[j],xmax), c(0,hx[j],0), col="steelblue1")
result <- paste("P(",cvupper,">= Chi Sq ) =",
   prob)
mtext(result,3)
 }

else if(df>=1)
{
xmin2 <- qchisq(.0005, df=df, ncp=0)
xmax2 <- qchisq(.9995, df=df, ncp=0)
.x <- seq(xmin2,xmax2,length=10000)
hx <- dchisq(.x,df)
ub=quantile
cvupper=round(ub,5)
prob <- round(pchisq(quantile,df=df,lower.tail=F),5)
plot(.x, hx, type="n", xlab="Chi Square Values", ylab="Density",
    main=paste("Chi Square Distribution, df=",df), axes=T,
	xlim=c(.9*xmin2,xmax2))
j <- .x >=ub
lines(.x, hx)
abline(h=0)
abline(v=0)
abline(v=quantile,col="red")
polygon(c(ub,.x[j],xmax2), c(0,hx[j],0), col="steelblue1")
result <- paste("P(",cvupper,">= Chi Sq ) =",
   prob)
mtext(result,3)
 }
}
