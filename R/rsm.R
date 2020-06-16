#' Robust Symmetry Plot
#'
#' A symmetry plot with simulated 95 per cent boundaries based on resampling
#' methods
#'
#' @param x \code{x} is the quantiative univariate RV of interest
#' @param alpha use to set confidence limits, default =.05
#' @param ...  Will accept some other graphical parameters

#' @author Bruce Dudek's modification of Vincent Zoonekynd's function
#'
#' @references \code{rsm} is based on a function provided by
#' \emph{http://zoonek2.free.fr/UNIX/48_R/03.html}
#' @examples
#'
#' data(howell2_1t)
#' rsm(howell2_1t$rxtime) # a reaction time variable
#'
#' @export rsm
#' @importFrom e1071 skewness
#' @importFrom quantreg lprq
#' @importFrom stats bw.nrd0
#' @importFrom graphics box
rsm <-
function (x, alpha = .05, ...) {
  main = paste("Robust Symmetry Plot; skewness =",
               round(e1071::skewness(x), digits=2))
  N = max(ceiling(1000/length(x)),2)
    cat(N, "\n")
    #require(quantreg)
    #require(e1071)
    # The symetry plot
    x <- x[!is.na(x)]
    n <- length(x)
    nn <- ceiling(n/2)
    x <- sort(x)
    d <- abs(x - median(x))       # Distance to the median
    plot( d[1:nn], d[n:(n-nn+1)],
          xlab = "Distance below the median",
          ylab = "Distance above the median",
          main = main,
          ... )
    # The symmmetry plot of resampled, symetric data
    y <- c(x, 2 * median(x) - x)  # We symetrize the data
    X <- Y <- rep(NA, N * nn)
    for (i in 1:N) {
      a <- sort(sample(y, n))
      a <- abs(a - median(a))
      j <- ((i-1) * nn + 1) : (i * nn)
      X[j] <- a[1:nn]
      Y[j] <- a[n:(n-nn+1)]
    }
    points(X, Y, col="red")
    points( d[1:nn], d[n:(n-nn+1)], ...)
    # The 5% confidence interval stemming from the resampled data
    for (tau in c(alpha, 1-alpha)) {
      r <- quantreg::lprq(X, Y,
                h = bw.nrd0(x),  # See ?density
                tau = tau)
      lines(r$xx, r$fv, col = "blue", lwd = 3)
    }
    abline(0, 1, col = "blue", lty = 2)
    # The histogram, in a corner
    op <- par(fig = if (e1071::skewness(x)>0)
                      c(.02,.5,.5,.98)       # Top left corner
                    else c(.5,.98,.02,.5),   # Bottom right
              new = TRUE)
    hist(x, probability=T,
        col="skyblue", xlab="", ylab="", main="", axes=F)
    lines(density(x), col="red", lwd=2)
    box()
    par(op)
  }
