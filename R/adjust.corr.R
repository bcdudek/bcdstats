#' Correlation Matrices and tests with p-value adjustment
#'
#' The \code{adjust.corr} function produces correlation matrices from raw data frames
#' and produces standard normal theory t-test based p-values for all correlations
#' among pairs of variables. It will also produce adjusted p-values for the
#' simultaneous inference on whole correlation matrices.
#'
#' @param x A data frame or a correlation matrix.  If there is missing data it is
#' safer to pass the original data and control how missing data are handled
#' with the \code{use} argument.
#' @param use specifies how missing values are handled: "complete.obs" peeforms
#' listwise deletion of cases and is the default; "pairwise.complete.obs"
#' performs pairwise deletion.
#' @param type "pearson" or "spearman" permits specification of the type of
#' correlation coefficient.
#' @param adjust Which p-value adjustment for multiple testing is to be used?
#' ("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none").
#' The "holm" method is the default and is preferred to the "bonferroni" method.
#' The "hochberg" and "hommel" methods assume independence of the multiple tests and that
#' is unlikely for correlation matrices. The "BH" and "fdr" methods are equivalent and
#' produce the false discovery rate adjustment.  See help for the \code{p.adjust} function
#' for details.
#'
#'
#' @details The function takes a data frame or a correlation matrix.  The former is preferred
#' to give control over the method of handling missing data.  The correlation matrix
#' is returned, along with unadjusted and adjusted p-value matrices. Adjustment for simultaneous
#' inference utilizes the\code{p.adjust} function from base R to accomplish the adjustments.
#' The p-values assume a two-tailed test.  Internally the function uses "Hmisc::rcorr" to
#' calculate the correlation coefficients and to handle pairwise deletion when missing data
#' are present.
#'
#' Similar capabilities can be found in the \code{psych::corr.test} and \code{psych::corr.p}
#' functions which will also produce confidence intervals for the correlation coefficients.
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @references This function is a modeled on  a function originally coded
#'    as the \code{corr.adjust} function in the \bold{RcmdrMisc} package.
#'
#' @examples
#' adjust.corr(mtcars[,c(1,4:8)], use="complete.obs", adjust="fdr")
#' adjust.corr(mtcars[,c(1,4:8)], use="pairwise.complete.obs")
#' data(attitude)
#' adjust.corr(attitude, adjust="none")
#' M2 <- cor(mtcars[,c(1,4:8)])
#' adjust.corr(M2)
#'
#' @importFrom Hmisc rcorr
#' @importFrom stats p.adjust
#' @export adjust.corr
adjust.corr <- function (x, type = c("pearson", "spearman"),
                      use = c("complete.obs",
                              "pairwise.complete.obs"),
                      adjust="holm")
{
  opt <- options(scipen = 5)
  on.exit(options(opt))
  type <- match.arg(type)
  use <- match.arg(use)
  x <- if (use == "complete.obs")
    as.matrix(na.omit(x))
  else as.matrix(x)
  R <- Hmisc::rcorr(x, type = type)
  P <- P.unadj <- R$P
  p <- P[lower.tri(P)]
  adj.p <- p.adjust(p, method = adjust)
  P[lower.tri(P)] <- adj.p
  P[upper.tri(P)] <- 0
  P <- P + t(P)
  P <- ifelse(P < 1e-04, 0, P)
  P <- format(round(P, 4))
  diag(P) <- ""
  P[c(grep("0.0000", P), grep("^ 0$", P))] <- "<.0001"
  P[grep("0.000$", P)] <- "<.001"
  P.unadj <- ifelse(P.unadj < 1e-04, 0, P.unadj)
  P.unadj <- format(round(P.unadj, 4))
  diag(P.unadj) <- ""
  P.unadj[c(grep("0.0000$", P.unadj), grep("^ 0$",
                                           P.unadj))] <- "<.0001"
  P.unadj[grep("0.000$", P.unadj)] <- "<.001"
  result <- list(R = R, P = P, P.unadj = P.unadj, type = type, adjustr=adjust)
  class(result) <- "adjust.corr"
  result
}
#' Print method for adjust.corr
#'
#' @param x An object produced by \code{adjust.corr}
#' @param ... not used
#'
#' @method print adjust.corr
#' @export
print.adjust.corr <- function (x, ...)
{
  cat("\n", if (x$type == "pearson")
    "Pearson"
    else "Spearman", "correlations:\n")
  print(round(x$R$r, 4))
  cat("\n Number of Variables: ")
  adjustr <- x$adjustr
  n <- x$R$n
  if (all(n[1] == n))
    cat(n[1], "\n")
  else {
    cat("\n")
    print(n)
  }
  cat("\n Pairwise unadjusted two-tailed p-values:\n")
  print(x$P.unadj, quote = FALSE)
  cat("\n Adjusted p-values (",adjustr," method )\n")
  print(x$P, quote = FALSE)
}
