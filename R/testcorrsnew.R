#' Test the difference between two overlapping correlations (Meng, Rosenthal & Rubin, 1992)
#'
#' Implements the Z1* test statistic of Meng, Rosenthal, and Rubin (1992) for
#' comparing two correlations that share a common variable, e.g. testing
#' whether \eqn{r_{12}} differs from \eqn{r_{13}} when both are computed on
#' the same sample and variable 1 is common to both. The test accounts for
#' the dependency between the two correlations induced by \eqn{r_{23}}, the
#' correlation between the two non-overlapping variables.
#'
#' @param r12 Correlation between variables 1 and 2.
#' @param r13 Correlation between variables 1 and 3.
#' @param r23 Correlation between variables 2 and 3.
#' @param n Sample size used to compute all three correlations.
#' @param alternative Character string specifying the alternative hypothesis:
#'   `"two.sided"` (default), `"less"`, or `"greater"`. `"less"`/`"greater"`
#'   refer to the sign of \eqn{r_{12} - r_{13}}.
#'
#' @return A list with class `"mengz1_test"` containing:
#'   \item{statistic}{The Z1* test statistic.}
#'   \item{p.value}{The associated p-value.}
#'   \item{r12, r13, r23, n}{The inputs.}
#'   \item{diff}{\eqn{r_{12} - r_{13}}.}
#'   \item{alternative}{The alternative hypothesis used.}
#'
#' @references
#' Meng, X. L., Rosenthal, R., & Rubin, D. B. (1992). Comparing correlated
#' correlation coefficients. *Psychological Bulletin*, 111(1), 172-175.
#'
#' @examples
#' mengz1_test(r12 = 0.5, r13 = 0.3, r23 = 0.4, n = 100)
#'
#' @export
mengz1_test <- function(r12, r13, r23, n,
                         alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)

  stopifnot(
    is.numeric(r12), is.numeric(r13), is.numeric(r23), is.numeric(n),
    length(r12) == 1, length(r13) == 1, length(r23) == 1, length(n) == 1,
    r12 >= -1, r12 <= 1, r13 >= -1, r13 <= 1, r23 >= -1, r23 <= 1,
    n > 3
  )

  # Fisher z transforms of the two correlations being compared
  z12 <- atanh(r12)
  z13 <- atanh(r13)

  # Average squared correlation and the f/h adjustment terms
  rbar2 <- (r12^2 + r13^2) / 2
  f <- pmin((1 - r23) / (2 * (1 - rbar2)), 1)
  h <- (1 - f * rbar2) / (1 - rbar2)

  z_stat <- (z12 - z13) * sqrt((n - 3) / (2 * (1 - r23) * h))

  p_value <- switch(
    alternative,
    "two.sided" = 2 * pnorm(-abs(z_stat)),
    "less" = pnorm(z_stat),
    "greater" = pnorm(z_stat, lower.tail = FALSE)
  )

  result <- list(
    statistic = z_stat,
    p.value = p_value,
    r12 = r12,
    r13 = r13,
    r23 = r23,
    n = n,
    diff = r12 - r13,
    alternative = alternative
  )
  class(result) <- "mengz1_test"
  result
}

#' @export
print.mengz1_test <- function(x, digits = 4, ...) {
  cat("Meng, Rosenthal & Rubin (1992) Z1* test for correlated correlations\n\n")
  cat(sprintf("r12 = %.*f, r13 = %.*f, r23 = %.*f, n = %d\n",
              digits, x$r12, digits, x$r13, digits, x$r23, x$n))
  cat(sprintf("difference (r12 - r13) = %.*f\n", digits, x$diff))
  cat(sprintf("Z = %.*f, p-value = %.*g (%s)\n",
              digits, x$statistic, digits, x$p.value, x$alternative))
  invisible(x)
}
