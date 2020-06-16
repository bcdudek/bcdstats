#' Graphical display for the Chi Squared distribution, given a specified
#' probability
#'
#' A function to produce a graph of a Chi Squared distribution with regions
#' shaded according to a specified probability.
#'
#' @param prob The tail probability value to define the region to be shaded on
#' the graph and the corresponding quantile to be displayed.
#' @param df The \code{df} for the \emph{Chi Squared} Distribution.  Default is
#' 1 df (see note below)
#' @note The scaling on plots of Chi Squared Distributions of varying degrees
#' of freedom requires considerable adjustment. When df=1, \code{qgchisq}
#' returns a two-paneled figure.  The plot on the left is a fairly full scale
#' plot of the distribution without shading of a tail region.  The plot on the
#' right has axes truncated so that scale expansion will permit visualization
#' of the relevant area under the curve.
#'
#' Also note that the \code{qgchisq} function only displays upper tail regions since
#' most inference with the Chi Squared distribution only requires upper tail
#' evaluation.  The lower tail probability is 1 minus the returned value.
#'
#' @section Warning: Specifying extremely high or low probabilities may not
#' yield visible shaded areas if the regions are beyond the limits of the X
#' axis scale of the graph.
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#' @seealso \code{\link[bcdstats:pgchisq]{pgchisq}}
#' @examples
#'
#' qgchisq(.05,df=1)
#' qgchisq(.05,df=2)
#' qgchisq(.05,df=25)
#' qgchisq(.05,df=250)
#' qgchisq(.01,df=1)
#' qgchisq(.01,df=2)
#' qgchisq(.01,df=25)
#' qgchisq(.01,df=250)
#'
#' @export qgchisq
#' @importFrom stats dchisq
#' @importFrom stats qchisq
#' @importFrom graphics layout
qgchisq <-
function(prob,df=1)
{

if(df==1)
{
xmin <- qchisq(.0005, df=1, ncp=0)
xmax <- qchisq(.999, df=1, ncp=0)
.x <- seq(xmin,xmax,length=1000)
hx <- dchisq(.x,df=1)
ub=qchisq(prob,df=1,lower.tail=F)
cvupper=round(ub,5)
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
abline(v=ub,col="red")
polygon(c(ub,.x[j],xmax), c(0,hx[j],0), col="steelblue1")
result <- paste("P(",cvupper,">= Chi Sq ) =",
   round(prob,5))
mtext(result,3)
 }

else if(df>=1)
{
xmin2 <- qchisq(.0005, df=df, ncp=0)
xmax2 <- qchisq(.9995, df=df, ncp=0)
.x <- seq(xmin2,xmax2,length=10000)
hx <- dchisq(.x,df)
ub=qchisq(prob,df,lower.tail=F)
cvupper=round(ub,5)
plot(.x, hx, type="n", xlab="Chi Square Values", ylab="Density",
    main=paste("Chi Square Distribution, df=",df), axes=T,
	xlim=c(.9*xmin2,xmax2))
j <- .x >=ub
lines(.x, hx)
abline(h=0)
abline(v=0)
abline(v=ub,col="red")
polygon(c(ub,.x[j],xmax2), c(0,hx[j],0), col="steelblue1")
result <- paste("P(",cvupper,">= Chi Sq ) =",
   round(prob,5))
mtext(result,3)
 }
}
