#' Test Two Correlations from Independent Groups
#'
#' Differences in Pearson correlations of two variables are tested when they
#' are measured in independent samples, for example r(xy) in group 1 vs r(xy)
#' in group 2.
#' The method employed is one that utilizes Fisher's Z transformation of the
#' Pearson correlation coefficients.  The test statistic is a standard normal
#' deviate.  The method can be found in standard textbook sources (\emph{e.g.},
#' Hays, 1993, Howell, 2013.)  If the user wishes to perform tests of two r's
#' from independent groups with standard methods that do no utilize Fisher's Z
#' transform, they are encouraged to use linear regression models that employ
#' an interaction term.
#'
#' @param r1 The Pearson correlation coefficient (rxy) in group 1.
#' @param r2 The Pearson correlation coefficient (rxy) in group 2.
#' @param n1 Sample size in group 1
#' @param n2 Sample size in group 1
#' @param twotailed The test can be either two- or one-tailed by specifying
#' \code{twotailed=T} or \code{twotailed=F}, respectively.
#'
#' @section Related Functions:  \code{test2r.ind} is a member of a set of
#' functions that provide tests of differences between independent and
#' dependent correlations.  The functions were inspired by the `paired.r`
#' function in the **psych** package and some of the code is modeled on code
#' from that function. See:
#' \itemize{
#' \item
#' \code{\link[bcdstats:test2r.t2]{test2r.t2}} Test two dependent correlations
#' with the the T2 method: r(yx1) vs r(yx2)
#' \item
#' \code{\link[bcdstats:test2r.mengz1]{test2r.mengz1}}, Test the difference between
#' two dependent correlations with the the Meng z1 method: r(yx1) vs r(yx2)in one
#' sample of cases.
#' \item
#' \code{\link[bcdstats:test2r.steigerz1]{test2r.steigerz1}}, Test the difference between
#' two dependent correlations with the the Steiger z1 method: r(yx1) vs r(yx2) in one
#' sample of cases.
#' \item
#' \code{\link[bcdstats:test2r.steigerz2]{test2r.steigerz2}}, Test the difference between
#' two dependent correlations with the the Steiger z2 method: r(jk) vs r(hm) in one
#' sample of cases.
#' \item
#' \code{\link[bcdstats:test2r.ind]{test2r.ind}}, the present function }
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @seealso Analysts are also encouraged to explore robust methods for
#' evaluation of correlation comparison hypotheses. For example, see work of R.
#' Wilcox (texts above and also
#' \emph{http://dornsife.usc.edu/labs/rwilcox/software/}
#' @references Cheung, M. W. L., & Chan, W. (2004). Testing dependent
#' correlation coefficients via structural equation modeling.
#' \emph{Organizational Research Methods}, 7(2), 206-223. \cr Dunn, O. J., &
#' Clark, V. (1971). Comparison of tests of the equality of dependent
#' correlation coefficients. \emph{Journal of the American Statistical
#' Association}, 66(336), 904-908. \cr Hays, W. L. (1994). \emph{Statistics}
#' (5th ed.). Fort Worth: Harcourt College Publishers.\cr Hendrickson, G. F.,
#' Stanley, J. C., & Hills, J. R. (1970). Olkin's new formula for significance
#' of r13 vs. r23 compared with Hotelling's method. \emph{American Educational
#' Research Journal}, 7(2), 189-195. \cr Hittner, J. B., May, K., & Silver, N.
#' C. (2003). A Monte Carlo evaluation of tests for comparing dependent
#' correlations. \emph{The Journal of general psychology}, 130(2), 149-168. \cr
#' Howell, D. C. (2013). \emph{Statistical methods for psychology} (8th ed.).
#' Belmont, CA: Wadsworth Cengage Learning.\cr Meng, X. L., Rosenthal, R., &
#' Rubin, D. B. (1992). Comparing correlated correlation coefficients.
#' \emph{Psychological Bulletin}, 111(1), 172-175. \cr Neill, J. J., & Dunn, O.
#' J. (1975). Equality of dependent correlation coefficients.
#' \emph{Biometrics}, 31(2), 531-543. \cr Olkin, I., & Finn, J. D. (1990).
#' Testing correlated correlations. \emph{Psychological Bulletin}, 108(2),
#' 330-333. \cr Silver, N. C., Hittner, J. B., & May, K. (2004). Testing
#' dependent correlations with nonoverlapping variables: A Monte Carlo
#' simulation. \emph{The Journal of experimental education}, 73(1), 53-69. \cr
#' Steiger, J. H. (1980). Tests for comparing elements of a correlation matrix.
#' \emph{Psychological Bulletin}, 87(2), 245-251. \cr Wilcox, R. R. (2012).
#' \emph{Introduction to robust estimation and hypothesis testing}
#'
#' @examples
#'
#' test2r.ind(.30,.35,n1=50,n2=60)
#' test2r.ind(.10,.45,n1=60,n2=80)
#' test2r.ind(.41,.59,n1=100,n2=105)
#' test2r.ind(.41,.59,n1=100,n2=105,twotailed=FALSE)
#'
#'
#' @export test2r.ind
test2r.ind <-
  function (r1, r2, n1, n2, twotailed = TRUE)
  {
    fishersz.r1 <- 0.5 * log((1 + r1)/(1 - r1))
    fishersz.r2 <- 0.5 * log((1 + r2)/(1 - r2))
    se.diff.r <- sqrt(1/(n1 - 3) + 1/(n2 - 3))
    diff <- fishersz.r1 - fishersz.r2
    z.teststat <- abs(diff/se.diff.r)
    p <- (1 - pnorm(z.teststat))
    if (twotailed)
      p <- 2 * p
    r1
    r2
    fishersz.r1
    fishersz.r2
    z.teststat
    p
    return(list(title = "test of difference between two independent correlations",
                title2 = "Using Fisher's Z transformation of pearson coefficients",
                Pearson.r.sample1=r1,Pearson.r.sample2=r2,
                Fishersz.r1=fishersz.r1, Fishersz.r2=fishersz.r2,z.teststatistic = z.teststat, p = p))
  }
NA

#' Test the difference between two dependent correlations with the the Meng z1
#' method
#'
#' Differences in Pearson correlations are tested with the Meng's Z1
#' modification of Dunn's method.  The test is appropriate when the
#' correlations are dependent.  More specifically r(yx1) is tested versus
#' r(yx2)in one sample of cases.  The function requires the three Pearson
#' product moment correlations between three variables called y, x1 and x2 in
#' the notation here.  At present, the function only performs a one-tailed
#' test.
#'
#' The Meng, et al., 1992 method uses the Fisher's Z transformation of the
#' Pearson correlation coefficients and produces a standard normal deviate.
#'
#' @param ry.x1 y is the variable common to the two correlations.  It is
#' labeled y since the most common usage of this test is when a dependent
#' variable (y) is correlated with two different independent variables (x1, and
#' x2).  This argument \code{ry.x1} is the first of the two correlations of y
#' with the IV's.
#' @param ry.x2 This argument \code{ry.x2} is the second of the two
#' correlations of y with the IV's.
#' @param rx1.x2 The function and test require the Pearson correlation between
#' the two X's as well.
#' @param n Sample Size
#'
#' @return \item{z }{The test statistic value, a 'z'.} \item{pvalue
#' }{one-tailed probability of the 'z' test statistic.}
#'
#' @section Related Functions: \code{test2r.mengz1} is a member of a set of
#' functions that provide tests of differences between independent and
#' dependent correlations.  The functions were inspired by the `paired.r`
#' function in the **psych** package and some of the code is modeled on code
#' from that function. See:
#' \itemize{
#' \item
#' \code{\link[bcdstats:test2r.t2]{test2r.t2}}, Test two dependent correlations
#' with the the T2 method: r(yx1) vs r(yx2)
#' \item
#' \code{\link[bcdstats:test2r.mengz1]{test2r.mengz1}}, the present function
#' \item
#' \code{\link[bcdstats:test2r.steigerz1]{test2r.steigerz1}}, Test the difference between
#' two dependent correlations with the the Steiger z1 method: r(yx1) vs r(yx2) in one
#' sample of cases.
#' \item
#' \code{\link[bcdstats:test2r.steigerz2]{test2r.steigerz2}}, Test the difference between
#' two dependent correlations with the the Steiger z2 method: r(jk) vs r(hm) in one
#' sample of cases.
#' \item
#' \code{\link[bcdstats:test2r.ind]{test2r.ind}} Test two r(xy) from
#'  Independent Groups
#'  }
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @seealso Analysts are also encouraged to explore robust methods for
#' evaluation of correlation comparison hypotheses. For example, see work of R.
#' Wilcox (texts above and also
#' \emph{http://dornsife.usc.edu/labs/rwilcox/software/}
#'
#' @references Cheung, M. W. L., & Chan, W. (2004). Testing dependent
#' correlation coefficients via structural equation modeling.
#' \emph{Organizational Research Methods}, 7(2), 206-223. \cr Dunn, O. J., &
#' Clark, V. (1971). Comparison of tests of the equality of dependent
#' correlation coefficients. \emph{Journal of the American Statistical
#' Association}, 66(336), 904-908. \cr Hays, W. L. (1994). \emph{Statistics}
#' (5th ed.). Fort Worth: Harcourt College Publishers.\cr Hendrickson, G. F.,
#' Stanley, J. C., & Hills, J. R. (1970). Olkin's new formula for significance
#' of r13 vs. r23 compared with Hotelling's method. \emph{American Educational
#' Research Journal}, 7(2), 189-195. \cr Hittner, J. B., May, K., & Silver, N.
#' C. (2003). A Monte Carlo evaluation of tests for comparing dependent
#' correlations. \emph{The Journal of general psychology}, 130(2), 149-168. \cr
#' Howell, D. C. (2013). \emph{Statistical methods for psychology} (8th ed.).
#' Belmont, CA: Wadsworth Cengage Learning.\cr Meng, X. L., Rosenthal, R., &
#' Rubin, D. B. (1992). Comparing correlated correlation coefficients.
#' \emph{Psychological Bulletin}, 111(1), 172-175. \cr Neill, J. J., & Dunn, O.
#' J. (1975). Equality of dependent correlation coefficients.
#' \emph{Biometrics}, 31(2), 531-543. \cr Olkin, I., & Finn, J. D. (1990).
#' Testing correlated correlations. \emph{Psychological Bulletin}, 108(2),
#' 330-333. \cr Silver, N. C., Hittner, J. B., & May, K. (2004). Testing
#' dependent correlations with nonoverlapping variables: A Monte Carlo
#' simulation. \emph{The Journal of experimental education}, 73(1), 53-69. \cr
#' Steiger, J. H. (1980). Tests for comparing elements of a correlation matrix.
#' \emph{Psychological Bulletin}, 87(2), 245-251. \cr Wilcox, R. R. (2012).
#' \emph{Introduction to robust estimation and hypothesis testing}
#'
#' @examples
#'
#' test2r.mengz1(.6,.31,.73,75)
#' test2r.mengz1(.45,.03,.65,100)
#' test2r.mengz1(.45,.03,.15,35)
#'
#' @export test2r.mengz1
test2r.mengz1 <-
  function (ry.x1, ry.x2, rx1.x2, n)
  {
    fz1 <- 0.5 * log((1 + ry.x1)/(1 - ry.x1))
    fz2 <- 0.5 * log((1 + ry.x2)/(1 - ry.x2))
    fz3 <- 0.5 * log((1 + rx1.x2)/(1 - rx1.x2))
    dif <- fz1-fz2
    avsq <- ((ry.x1^2) + (ry.x2^2))/2
    f <- (1-(rx1.x2))/(2*(1-avsq))
    h <- (1-f*avsq)/(1-avsq)
    rad <- (12/(2*(1-rx1.x2)*h))^.5
    z2 <- dif*rad

    zteststat = z2
    p <- pnorm(abs(zteststat),0,1, lower.tail = FALSE)
    two_p <- 2*p
    #if (twotailed)
    #    p <- 2 * p
    return(list(test1 = "Dunn test of difference between dependent correlations r(yx1) and r(yx2)",
                test2= "As implemented by Meng, et al 1992",
                ry.x1=ry.x1, ry.x2=ry.x2,rx1.x2=rx1.x2,
                Difference_between_fishersz_correlations=dif,
                z.teststatistic = zteststat,onetail_p_value = p,twotail_p_value=two_p))

  }
NA

#' Test the difference between two dependent correlations with the the
#' Steiger's z1 method
#'
#' Differences in Pearson correlations are tested with the Steiger's Z1 method.
#' The test is appropriate when the correlations are dependent.  More
#' specifically r(yx1) is tested versus r(yx2)in one sample of cases.  The
#' function requires the three Pearson product moment correlations between
#' three variables called y, x1 and x2 in the notation here.  Both one- and
#' two-tailed p-values are returned.
#'
#' The Steiger1980 method uses the Fisher's Z transformation of the Pearson
#' correlation coefficients and produces a standard normal deviate.
#'
#' @param ry.x1 y is the variable common to the two correlations.  It is
#' labeled y since the most common usage of this test is when a dependent
#' variable (y) is correlated with two different independent variables (x1, and
#' x2).  This argument \code{ry.x1} is the first of the two correlations of y
#' with the IV's.
#' @param ry.x2 This argument \code{ry.x2} is the second of the two
#' correlations of y with the IV's.
#' @param rx1.x2 The function and test require the Pearson correlation between
#' the two X's as well.
#' @param n Sample Size
#'
#' @return \item{z }{The test statistic value, a 'z'.} \item{pvalue }{one- and
#' two-tailed probabilities of the 'z' test statistic.}
#'
#' @section Related Functions: \code{test2r.steigerz1} is a member of a set of
#' functions that provide tests of differences between independent and
#' dependent correlations.  The functions were inspired by the `paired.r`
#' function in the **psych** package and some of the code is modeled on code
#' from that function. See:
#' \itemize{ \item
#' \code{\link[bcdstats:test2r.t2]{test2r.t2}}, Test two dependent correlations
#' with the the T2 method: r(yx1) vs r(yx2)
#' \item
#' \code{\link[bcdstats:test2r.mengz1]{test2r.mengz1}}, Test the difference between
#' two dependent correlations with the the Meng z1 method: r(yx1) vs r(yx2)in one
#' sample of cases.
#' \item
#' \code{\link[bcdstats:test2r.steigerz1]{test2r.steigerz1}}, the present
#' function
#' \item
#' \code{\link[bcdstats:test2r.steigerz2]{test2r.steigerz2}}, Test the difference between
#' two dependent correlations with the the Steiger z2 method: r(jk) vs r(hm) in one
#' sample of cases.
#' \item
#' \code{\link[bcdstats:test2r.ind]{test2r.ind}}, Test two r(xy) from
#'  Independent Groups }
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @seealso Analysts are also encouraged to explore robust methods for
#' evaluation of correlation comparison hypotheses. For example, see work of R.
#' Wilcox (texts above and also
#' \emph{http://dornsife.usc.edu/labs/rwilcox/software/}
#'
#' @references Cheung, M. W. L., & Chan, W. (2004). Testing dependent
#' correlation coefficients via structural equation modeling.
#' \emph{Organizational Research Methods}, 7(2), 206-223. \cr Dunn, O. J., &
#' Clark, V. (1971). Comparison of tests of the equality of dependent
#' correlation coefficients. \emph{Journal of the American Statistical
#' Association}, 66(336), 904-908. \cr Hays, W. L. (1994). \emph{Statistics}
#' (5th ed.). Fort Worth: Harcourt College Publishers.\cr Hendrickson, G. F.,
#' Stanley, J. C., & Hills, J. R. (1970). Olkin's new formula for significance
#' of r13 vs. r23 compared with Hotelling's method. \emph{American Educational
#' Research Journal}, 7(2), 189-195. \cr Hittner, J. B., May, K., & Silver, N.
#' C. (2003). A Monte Carlo evaluation of tests for comparing dependent
#' correlations. \emph{The Journal of general psychology}, 130(2), 149-168. \cr
#' Howell, D. C. (2013). \emph{Statistical methods for psychology} (8th ed.).
#' Belmont, CA: Wadsworth Cengage Learning.\cr Meng, X. L., Rosenthal, R., &
#' Rubin, D. B. (1992). Comparing correlated correlation coefficients.
#' \emph{Psychological Bulletin}, 111(1), 172-175. \cr Neill, J. J., & Dunn, O.
#' J. (1975). Equality of dependent correlation coefficients.
#' \emph{Biometrics}, 31(2), 531-543. \cr Olkin, I., & Finn, J. D. (1990).
#' Testing correlated correlations. \emph{Psychological Bulletin}, 108(2),
#' 330-333. \cr Silver, N. C., Hittner, J. B., & May, K. (2004). Testing
#' dependent correlations with nonoverlapping variables: A Monte Carlo
#' simulation. \emph{The Journal of experimental education}, 73(1), 53-69. \cr
#' Steiger, J. H. (1980). Tests for comparing elements of a correlation matrix.
#' \emph{Psychological Bulletin}, 87(2), 245-251. \cr Wilcox, R. R. (2012).
#' \emph{Introduction to robust estimation and hypothesis testing}
#'
#' @examples
#'
#' test2r.steigerz1(.6,.31,.73,75)
#' test2r.steigerz1(.45,.03,.65,100)
#' test2r.steigerz1(.45,.03,.15,35)
#'
#' @export test2r.steigerz1
test2r.steigerz1 <-
  function (ry.x1, ry.x2, rx1.x2, n)
  {
    fz1 <- 0.5 * log((1 + ry.x1)/(1 - ry.x1))
    fz2 <- 0.5 * log((1 + ry.x2)/(1 - ry.x2))
    fz3 <- 0.5 * log((1 + rx1.x2)/(1 - rx1.x2))
    dif <- fz1-fz2
    av <- (ry.x1 + ry.x2)/2
    covnum1 <- rx1.x2 * (1-(av^2)-(av^2))
    covnum2 <- (.5*(av^2)) * (1-(av^2)-(av^2)-(rx1.x2^2))
    covdenom <- (1-(av^2))^2
    cov <- (covnum1-covnum2)/covdenom
    zteststat = (((n-3)^.5)*dif)/((2-(2*cov))^.5)
    p <- pnorm(abs(zteststat),0,1, lower.tail = FALSE)
    two_p <- p*2
    #if (twotailed)
    #    p <- 2 * p
    return(list(test = "test of difference between dependent correlations r(yx1) and r(yx2)",
                ry.x1=ry.x1, ry.x2=ry.x2,rx1.x2=rx1.x2,cov=cov,num1=covnum1,num2=covnum2,denom=covdenom,
                Difference_between_fishersz_correlations=dif,
                z.teststatistic = zteststat,onetail_p_value = p,twotail_p_value=two_p))

  }
NA

#' Test the difference between two dependent correlations with the the
#' Steiger's z2 method
#'
#' Differences in Pearson correlations are tested with the Steiger's Z2 method.
#' The test is appropriate when the correlations are dependent.  More
#' specifically r(jk) is tested versus r(hm)in one sample of cases.  Thus there
#' are four different variables involved in the analysis.  The function
#' requires the input of the six possible bivariate Pearson product-moment
#' correlations among the four variables.  One, and two-tailed tests are available.
#'
#'
#' @param rjk The pearson product moment correlation that is to be tested
#' against \code{rhm} .
#' @param rhm The pearson product moment correlation that is to be tested
#' against \code{rjk}.
#' @param rjh One of the remaining four zero-order correlations among variables
#' j,k,h, and m.
#' @param rkh One of the remaining four zero-order correlations among variables
#' j,k,h, and m.
#' @param rkm One of the remaining four zero-order correlations among variables
#' j,k,h, and m.
#' @param rjm One of the remaining four zero-order correlations among variables
#' j,k,h, and m.
#' @param n Sample Size
#' @param twotailed The test can be two-tailed (\code{twotailed=TRUE}) or
#' one-tailed (\code{twotailed=FALSE}).  The default is two-tailed.
#'
#' @return \item{z }{The test statistic value, a standard normal deviate (z'.)}
#' \item{pvalue }{the one- or two-tailed probability of the 't'.}
#'
#' @section Related Functions:  \code{test2r.steigerz2} is a member of a set of
#' functions that provide tests of differences between independent and
#' dependent correlations.  The functions were inspired by the `paired.r`
#' function in the **psych** package and some of the code is modeled on code
#' from that function. See:
#' \itemize{
#' \item
#' \code{\link[bcdstats:test2r.t2]{test2r.t2}}, Test two dependent correlations with the the T2
#' method: r(yx1) vs r(yx2)
#' \item
#' \code{\link[bcdstats:test2r.mengz1]{test2r.mengz1}}, Test the difference between
#' two dependent correlations with the the Meng z1 method: r(yx1) vs r(yx2)in one
#' sample of cases.
#' \item
#' \code{\link[bcdstats:test2r.steigerz1]{test2r.steigerz1}}, Test the difference between
#' two dependent correlations with the the Steiger z1 method: r(yx1) vs r(yx2) in one
#' sample of cases.
#' \item
#' \code{\link[bcdstats:test2r.steigerz2]{test2r.steigerz2}}, the present
#' function
#' \item
#' \code{\link[bcdstats:test2r.ind]{test2r.ind}} Test two r(xy) from
#'  Independent Groups }
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @seealso Analysts are also encouraged to explore robust methods for
#' evaluation of correlation comparison hypotheses. For example, see work of R.
#' Wilcox (texts above and also
#' \emph{http://dornsife.usc.edu/labs/rwilcox/software/}
#'
#' @references Cheung, M. W. L., & Chan, W. (2004). Testing dependent
#' correlation coefficients via structural equation modeling.
#' \emph{Organizational Research Methods}, 7(2), 206-223. \cr Dunn, O. J., &
#' Clark, V. (1971). Comparison of tests of the equality of dependent
#' correlation coefficients. \emph{Journal of the American Statistical
#' Association}, 66(336), 904-908. \cr Hays, W. L. (1994). \emph{Statistics}
#' (5th ed.). Fort Worth: Harcourt College Publishers.\cr Hendrickson, G. F.,
#' Stanley, J. C., & Hills, J. R. (1970). Olkin's new formula for significance
#' of r13 vs. r23 compared with Hotelling's method. \emph{American Educational
#' Research Journal}, 7(2), 189-195. \cr Hittner, J. B., May, K., & Silver, N.
#' C. (2003). A Monte Carlo evaluation of tests for comparing dependent
#' correlations. \emph{The Journal of general psychology}, 130(2), 149-168. \cr
#' Howell, D. C. (2013). \emph{Statistical methods for psychology} (8th ed.).
#' Belmont, CA: Wadsworth Cengage Learning.\cr Meng, X. L., Rosenthal, R., &
#' Rubin, D. B. (1992). Comparing correlated correlation coefficients.
#' \emph{Psychological Bulletin}, 111(1), 172-175. \cr Neill, J. J., & Dunn, O.
#' J. (1975). Equality of dependent correlation coefficients.
#' \emph{Biometrics}, 31(2), 531-543. \cr Olkin, I., & Finn, J. D. (1990).
#' Testing correlated correlations. \emph{Psychological Bulletin}, 108(2),
#' 330-333. \cr Silver, N. C., Hittner, J. B., & May, K. (2004). Testing
#' dependent correlations with nonoverlapping variables: A Monte Carlo
#' simulation. \emph{The Journal of experimental education}, 73(1), 53-69. \cr
#' Steiger, J. H. (1980). Tests for comparing elements of a correlation matrix.
#' \emph{Psychological Bulletin}, 87(2), 245-251. \cr Wilcox, R. R. (2012).
#' \emph{Introduction to robust estimation and hypothesis testing}
#'
#' @examples
#'
#' test2r.steigerz2(.6,.25,.4,.4,.4,.4,n=125)
#' test2r.steigerz2(.6,.25,.4,.4,.4,.4,n=125,twotailed=TRUE)
#' test2r.steigerz2(.6,.25,.4,.4,.4,.4,n=125,twotailed=FALSE)
#' test2r.steigerz2(.75,.50,.5,.4,.3,.76,n=100,twotailed=TRUE)
#' test2r.steigerz2(.75,.50,.5,.4,.3,.76,n=40,twotailed=TRUE)
#'
#' @export test2r.steigerz2
test2r.steigerz2 <-
  function (rjk, rhm, rjh, rkh, rkm, rjm, n, twotailed=TRUE)
  {
    fzjk <- 0.5 * log((1 + rjk)/(1 - rjk))
    fzhm <- 0.5 * log((1 + rhm)/(1 - rhm))
    ravg <- (rjk+rhm)/2
    diff=fzjk-fzhm
    psi.jkhm <- .5*(((rjh-(ravg*rkh))*(rkm-(rkh*ravg)))
                    + ((rjm-(rjh*ravg))*(rkh-(ravg*rjh)))
                    + ((rjh-(rjm*ravg))*(rkm-(ravg*rjm)))
                    + ((rjm-(ravg*rkm))*(rkh-(rkm*ravg))))
    cjkhm <-psi.jkhm/((1-rjk^2)*(1-rhm^2))
    c2jkhm <- psi.jkhm/((1-ravg^2)*(1-ravg^2))
    zteststat <- (((n-3)^.5)*(fzjk-fzhm))/(2-((2*c2jkhm)^.5))

    p <- pnorm(abs(zteststat),0,1, lower.tail = FALSE)
    if (twotailed)
      p <- 2 * p
    return(list(test = "test of difference between dependent
                    correlations r12(called rjk) and r34 (called rhm)",
                samplesize=n,
                rjk=rjk,
                rhm=rhm,
                rjh=rjh, rkh=rkh, rkm=rkm, rjm=rjm,
                Difference_between_fishersz_correlations=diff,
                z.teststatistic = zteststat,p_value = p))

  }
NA

#' Test the difference between two dependent correlations with the the T2
#' method
#'
#' Differences in Pearson correlations are tested with the Williams T2
#' modification of Hoetellings method.  The test is appropriate when the
#' correlations are dependent.  More specifically r(yx1) is tested versus
#' r(yx2)in one sample of cases.  The function requires the three Pearson
#' product moment correlations between three variables called y, x1 and x2 in
#' the notation here.
#'
#'
#' @param ry.x1 y is the variable common to the two correlations.  It is
#' labeled y since the most common usage of this test is when a dependent
#' variable (y) is correlated with two different independent variables (x1, and
#' x2).  This argument \code{ry.x1} is the first of the two correlations of y
#' with the IV's.
#' @param ry.x2 This argument \code{ry.x2} is the second of the two
#' correlations of y with the IV's.
#' @param rx1.x2 The function and test require the Pearson correlation between
#' the two X's as well.
#' @param n Sample Size
#' @param twotailed The test can be two-tailed (\code{twotailed=TRUE}) or
#' one-tailed (\code{twotailed=FALSE}).  The default is two-tailed.
#'
#' @return \item{t }{The test statistic value, a 't'.} \item{df }{degrees of
#' freedom for the 't' (n-3).} \item{pvalue }{the one- or two-tailed
#' probability of the 't'.}
#'
#' @section Related Functions: \code{test2r.t2} is a member of a set of
#' functions that provide tests of differences between independent and
#' dependent correlations.  See: \itemize{
#' \item
#' \code{\link[bcdstats:test2r.t2]{test2r.t2}}, the present function
#' \item
#' \code{\link[bcdstats:test2r.mengz1]{test2r.mengz1}}, Test the difference between
#' two dependent correlations with the the Meng z1 method: r(yx1) vs r(yx2)in one
#' sample of cases.
#' \item
#' \code{\link[bcdstats:test2r.steigerz1]{test2r.steigerz1}}, Test the difference between
#' two dependent correlations with the the Steiger z1 method: r(yx1) vs r(yx2) in one
#' sample of cases.
#' \item
#' \code{\link[bcdstats:test2r.steigerz2]{test2r.steigerz2}}, Test the difference between
#' two dependent correlations with the the Steiger z2 method: r(jk) vs r(hm) in one
#' sample of cases.
#' #' \item
#' \code{\link[bcdstats:test2r.ind]{test2r.ind}} Test two r(xy) from
#'  Independent Groups }
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @seealso Analysts are also encouraged to explore robust methods for
#' evaluation of correlation comparison hypotheses. For example, see work of R.
#' Wilcox (texts above and also
#' \emph{http://dornsife.usc.edu/labs/rwilcox/software/}
#'
#' @references Cheung, M. W. L., & Chan, W. (2004). Testing dependent
#' correlation coefficients via structural equation modeling.
#' \emph{Organizational Research Methods}, 7(2), 206-223. \cr Dunn, O. J., &
#' Clark, V. (1971). Comparison of tests of the equality of dependent
#' correlation coefficients. \emph{Journal of the American Statistical
#' Association}, 66(336), 904-908. \cr Hays, W. L. (1994). \emph{Statistics}
#' (5th ed.). Fort Worth: Harcourt College Publishers.\cr Hendrickson, G. F.,
#' Stanley, J. C., & Hills, J. R. (1970). Olkin's new formula for significance
#' of r13 vs. r23 compared with Hotelling's method. \emph{American Educational
#' Research Journal}, 7(2), 189-195. \cr Hittner, J. B., May, K., & Silver, N.
#' C. (2003). A Monte Carlo evaluation of tests for comparing dependent
#' correlations. \emph{The Journal of general psychology}, 130(2), 149-168. \cr
#' Howell, D. C. (2013). \emph{Statistical methods for psychology} (8th ed.).
#' Belmont, CA: Wadsworth Cengage Learning.\cr Meng, X. L., Rosenthal, R., &
#' Rubin, D. B. (1992). Comparing correlated correlation coefficients.
#' \emph{Psychological Bulletin}, 111(1), 172-175. \cr Neill, J. J., & Dunn, O.
#' J. (1975). Equality of dependent correlation coefficients.
#' \emph{Biometrics}, 31(2), 531-543. \cr Olkin, I., & Finn, J. D. (1990).
#' Testing correlated correlations. \emph{Psychological Bulletin}, 108(2),
#' 330-333. \cr Silver, N. C., Hittner, J. B., & May, K. (2004). Testing
#' dependent correlations with nonoverlapping variables: A Monte Carlo
#' simulation. \emph{The Journal of experimental education}, 73(1), 53-69. \cr
#' Steiger, J. H. (1980). Tests for comparing elements of a correlation matrix.
#' \emph{Psychological Bulletin}, 87(2), 245-251. \cr Wilcox, R. R. (2012).
#' \emph{Introduction to robust estimation and hypothesis testing}
#'
#' @examples
#'
#' test2r.t2(.6,.4,.3,75)
#' test2r.t2(.6,.4,.3,75,twotailed=TRUE)
#' test2r.t2(.6,.4,.3,75,twotailed=FALSE)
#' test2r.t2(.45,.03,.65,100)
#' test2r.t2(.45,.03,.15,35,twotailed=FALSE)
#'
#' @export test2r.t2
test2r.t2 <-
  function (ry.x1, ry.x2, rx1.x2, n, twotailed = TRUE)
  {
    diff <- ry.x1 - ry.x2
    determin = 1 - ry.x1 * ry.x1 - ry.x2 * ry.x2 - rx1.x2 * rx1.x2 + 2 * ry.x1 *
      ry.x2 * rx1.x2
    av = (ry.x1 + ry.x2)/2
    cube = (1 - rx1.x2) * (1 - rx1.x2) * (1 - rx1.x2)
    radical = (((n-1)*(1+rx1.x2))/((2*((n-1)/(n-3))*determin)+(av*av*cube)))^.5
    t2 = diff*radical
    p <- pt(abs(t2), n - 3, lower.tail = FALSE)
    if (twotailed)
      p <- 2 * p
    return(list(test = "test of difference between two dependent correlations r(yx1) and r(yx2)",
                ry.x1=ry.x1, ry.x2=ry.x2,rx1.x2=rx1.x2,
                Difference_between_correlations=diff,
                t.teststatistic = t2, df=n-3, p_value = p, determin=determin))

  }
