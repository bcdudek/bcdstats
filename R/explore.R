#' Explore, an EDA function for Univariate Description and Display
#'
#' The function returns a six paneled figure with histograms, kernel density
#' functions, Boxplots with rugplots, normal probability plots, symmetry plots
#' and displays Interquartile ranges under a kernel density function.
#'
#' The function also uses the \code{"describe"} function from the "psych"
#' package to compute descriptive statistics for the RV.  The skewness and
#' kurtosis coefficients from \code{"describe"} are the \emph{G1} and \emph{G2}
#' coefficients.
#'
#' %% ~~ If necessary, more details than the description above ~~
#'
#' @param x The quantitative Random Variable of interest
#' @param na.rm missing data are handled by this function
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @examples
#'
#' explore(faithful$eruptions)
#' data(howell2_1)
#' explore(howell2_1t$rxtime)
#'
#' @export explore
#' @importFrom KernSmooth dpih
#' @importFrom graphics par
#' @importFrom graphics hist
#' @importFrom graphics lines
#' @importFrom stats density
#' @importFrom stats dnorm
#' @importFrom graphics rug
#' @importFrom graphics plot
#' @importFrom graphics abline
#' @importFrom graphics boxplot
#' @importFrom stats qqnorm
#' @importFrom stats qqline
#' @importFrom stats quantile
#' @importFrom stats median
#' @importFrom graphics polygon
#' @importFrom graphics mtext
#' @importFrom psych describe
explore <-
function(x,na.rm=T)
{
opar <- par(mfrow=c(2,3))
par(mai=c(0.6732, 0.5412, 0.9, 0.2772))
h1 <- KernSmooth::dpih(x[!is.na(x)])
bins <- seq(min(x,na.rm=T)-h1, max(x,na.rm=T)+h1, by=h1)
hist(x, main="Histogram with Kernel Density",
      col = "sky blue",xlab="Kernel Bandwidth based on Wand, 1996",
      probability = "TRUE",breaks=bins)
#      ylim = c(0, 1.5*max(density(x,na.rm=T)$y)))
lines(density(x,na.rm=T),
      col = "red",
      lwd = 3)
h<-hist(x, col="sky blue", breaks=bins,
   xlab="",
   main="Histogram with Normal Curve")
xfit<-seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),length=64)
yfit<-dnorm(xfit,mean=mean(x,na.rm=TRUE),sd=sd(x,na.rm=TRUE))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="red", lwd=2)
boxplot(x,notch=TRUE,col="dodgerblue", main="Boxplot with Jittered Rug")
rug(jitter(x,amount=.06),col="black",side=4,lwd=.01)
sym.plot <- function (x,
                                 N = max(ceiling(1000/length(x)),2),
                                 alpha = .05,
                                 xlab = "Distance below the median",
                                 ylab = "Distance above",
                                 main = paste("Symmetry Plot; skewness =", round(e1071::skewness(x), digits=2)),
                                 ...) {
  #cat(N, "\n")
  # The symmetry plot
  x <- x[!is.na(x)]
  n <- length(x)
  nn <- ceiling(n/2)
  x <- sort(x)
  d <- abs(x - median(x))       # Distance to the median
  plot( d[1:nn], d[n:(n-nn+1)],
        xlab = xlab, ylab = ylab,
        main = main,pch=16,
        ... )
  abline(0, 1, col = "blue", lty = 2,lwd=2)
}
sym.plot(x)
qqnorm(x)
qqline(x,col="red")
qdens.it <- function (x, ylab="Density", main="Quartiles on Kernel Density") {
  d1 <- density(x,na.rm=TRUE)
  plot(d1, type='l', ylab=ylab, main=main)
  q <- quantile(x,na.rm=T)
  qdens.it <- function (i, col) {
    x <- d1$x[i]
    y <- d1$y[i]
    polygon( c(x,rev(x)), c(rep(0,length(x)),rev(y)), border=NA, col=col )
  }
  qdens.it(d1$x <= q[2], 'skyblue1')
  qdens.it(q[2] <= d1$x & d1$x <= q[3], 'dodgerblue')
  qdens.it(q[3] <= d1$x & d1$x <= q[4], 'skyblue3')
  qdens.it(d1$x >= q[4], 'dodgerblue3')
  lines(d1, lwd=3)
}
qdens.it(x)
# define header for multiple graph display
mtext(text=paste0("Univariate Plots: ",deparse(substitute(x))),outer=T,line=-1.8,cex=1.4)
psych::describe(x,skew=TRUE,type=2)
}
