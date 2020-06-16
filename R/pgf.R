#' Graphical display for the F distribution, given a specified quantile
#'
#' A function to draw an F distribution and shade a region defined by the
#' quantile.  Note that pgf only gives upper tail probabilities and upper tail
#' shading of the relevant area under the curve.
#'
#'
#' @param quantile The quantile value (F value) to be evaluated.
#' @param df1 Numerator degrees of freedom for the \emph{F} distribution.  Note
#' that the default is 1.
#' @param df2 Denominator degrees of freedom for the \emph{F} distribution.
#' Note that the default is 25.
#' @note This function can be very helpful in instructional situations and can
#' replace the "F table" from textbooks.
#'
#' Warning: Specifying extremely high quantiles may not yield visible shaded
#' areas if the quantile is in a region where the distribution approaches a
#' density of zero. \cr In addition, probabilities are rounded to five decimal
#' places and will display as zero if below .000005.  Use the \code{pf}
#' function in this case.
#'
#' Note that the \code{pgf} function only displays upper tail
#' regions since most inference with the F distribution
#' only requires upper tail evaluation.  The lower tail probability can be
#' found as 1 minus the returned upper tail probability.
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @examples
#' pgf(3.75, df1=1,df2=25)
#' pgf(3.75, df1=2,df2=50)
#' pgf(3.75, df1=1,df2=125)
#' pgf(3.75, df1=6,df2=80)
#' pgf(4.11, df1=1,df2=18)
#' pgf(.65, df1=1,df2=25)
#' @export pgf
#' @importFrom stats qf
#' @importFrom stats pf
#' @importFrom stats df
pgf <-
function(quantile,df1=1,df2=5)
  {
    xmin <- qf(.0005, df1=df1,df2=df2)
    xmax <- qf(.9995, df1=df1,df2=df2)
    .x <- seq(xmin,xmax,length=10000)
    hx <- df(.x,df1,df2)
    ub <-quantile
    #cvupper<-round(ub,5)

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
    prob <- round((pf(quantile,df1,df2,lower.tail=F)),5)
    plot(.x, hx, type="n", xlab="F Values", ylab="Density",
         main=paste("F Distribution, df1=",df1, "df2=",df2), axes=T,
         xlim=c(xmin,rightedge),ylim=c(0,ymax))
    j <- .x >=ub
    lines(.x, hx)
    abline(h=0)
    abline(v=0)
    abline(v=ub,col="red")
    polygon(c(ub,.x[j],xmax), c(0,hx[j],0), col="steelblue1")
    result <- paste("P(",ub,">= F ) =",
                    prob)
    mtext(result,3)
    # qgf was modeled, in part on the function to draw
    # an F distribution found in the Rcmdr package
  }
