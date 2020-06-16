#' Graphical display for the F distribution, given a specified probability
#'
#' A function to produce a graph of an F distribution with regions shaded
#' according to a specified probability.
#'
#' @param prob The upper tail probability value to define the region to be
#' shaded on the graph and the corresponding quantile to be displayed.
#' @param df1 Numberator degrees of freedom for the \emph{F} distribution.
#' Note that the default is 1.
#' @param df2 Numberator degrees of freedom for the \emph{F} distribution.
#' Note that the default is 25.
#'
#' @note This function can be very helpful in instructional situations and can
#' replace the "F table" from textbooks.
#'
#' Also note that the \code{qgf} function only displays upper tail regions since
#' most inference with the F distribution only requires upper tail
#' evaluation.  The lower tail probability is 1 minus the returned value.
#'
#' @section Warning: Specifying extremely small probabilities may not yield
#' visible shaded areas if the quantile is in a region of the graph where the
#' density approaches zero.
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#' @examples
#'
#' qgf(.05,df1=1,df2=25)
#' qgf(.05,df1=6,df2=80)
#' qgf(.35,df1=1,df2=25)
#' qgf(.01,df1=3,df2=32)
#'
#' @export qgf
#' @importFrom stats qf
#' @importFrom stats df
qgf <-
function(prob,df1=1,df2=5)
{
xmin <- qf(.0005, df1=df1,df2=df2)
xmax <- qf(.9995, df1=df1,df2=df2)
.x <- seq(xmin,xmax,length=10000)
hx <- df(.x,df1,df2)
ub<-qf(prob,df1=df1,df2=df2,lower.tail=F)
cvupper<-round(ub,5)

if(ub <= (df2/(df2-2)))
{
  rightedge <- qf(.99, df1=df1,df2=df2)
}

else if(ub > 1)
{
  rightedge <- ub*(2.15**1.05)
}

if(df1<=6)
{
  ymax=.6
}
else if(df1>6)
{
  ymax=(max(hx)+.05)
}

plot(.x, hx, type="n", xlab="F Values", ylab="Density",
    main=paste("F Distribution, df1=",df1, "df2=",df2), axes=T,
	xlim=c(xmin,rightedge),ylim=c(0,ymax))
j <- .x >=ub
lines(.x, hx)
abline(h=0)
abline(v=0)
abline(v=cvupper,col="red")
polygon(c(ub,.x[j],xmax), c(0,hx[j],0), col="steelblue1")
result <- paste("P(",cvupper,">= F ) =",
   prob)
mtext(result,3)
# qgf was modeled, in part on the function to draw
# an F distribution found in the Rcmdr package
}
