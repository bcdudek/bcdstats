#' Regression Information for lm class objects
#'
#' The \code{mrinfo} function produces a wealth of additional information
#' on multiple regression models above and beyond what summary(), anova(),
#' and Anova() produce.
#'
#' @param lm.fit A \code{lm} class model object.
#' @param minimal specifies volume of returned information (see details). Default=T.
#' @param cilevel  Confidence interval level.  Default is .95.
#'
#' @details The function takes an \code{lm} fit object.  Supplemental information is
#' calculated for \code{lm} fit objects.  When \code{minimal=T} is specified,
#' a data frame of several indices (see values) is returned.  When \code{minimal=F},
#' a list of information, including the supplemental values is returned:
#' zero-order correlations, the coefficients table, confidence intervals for the
#' regression coefficients, type I SS anova table and type III SS Anova table.
#'
#' @section Values of the Supplemental Information:
#' \tabular{ll}{
#'   \code{beta wt} \tab Standardized Regression Coefficients\cr
#'   \code{partial r} \tab Partial correlations of IVs with the DV\cr
#'   \code{semi-partial r} \tab Semi-partial correlations of IVs with the DV\cr
#'   \code{tolerances} \tab Tolerance for each IV\cr
#'   \code{unique} \tab Unique proportion of variance in DV accounted for
#'   by each IV\cr
#'   \code{common}  \tab Common proportion of variance in DV accounted for
#'   by each IV shared with other IVs\cr
#'   \code{total} \tab Total proportion of variance in DV accounted for
#'   by each IV\cr
#'   \code{pearsons}  \tab The zero-order pearson correlation matrix among
#'   all variables\cr
#'   }
#'
#'@section Warnings:
#'
#'The \code{mrinfo} function is designed to work with multiple regression objects
#'where an intercept is estimated.  Models where the intercept is forced through
#'the origin ('no intercept' models) are problematic for interpretation of the
#'supplemental information listed above and it is not returned.
#'
#'Simple regression models do not require the supplemental information, but the
#'user can specify \code{minimal=F} to obtain the longer list of detailed information.
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @references This function is a modification of a function originally coded
#'    in the \code{regr} function in the \bold{yhat} package.  It uses the
#'    \code{effect.size} and \code{commonalityCoefficients} functions from that package.
#'
#' @examples
#' data(attitude)
#' fit1 <- lm(rating ~ complaints + learning +  privileges, data=attitude)
#' #summary(fit1)
#' mrinfo(fit1, minimal=TRUE, cilevel=.99)
#' mrinfo(fit1, minimal=FALSE, cilevel=.95)
#'
#' @export mrinfo
#' @importFrom yhat commonalityCoefficients
#' @importFrom yhat effect.size
#' @importFrom stats coef
#' @importFrom stats anova
#' @importFrom stats confint
#' @importFrom stats cor
#' @importFrom stats fitted.values
#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom stats na.omit
#' @importFrom car vif
#' @importFrom car Anova
#'
mrinfo <- function (lm.fit,
                    cilevel = .95,
                    minimal = TRUE)
{
  #require(car)
  #require(yhat)
  if (names(lm.fit$coefficients)[1] == "(Intercept)") {
    test1 = TRUE
  } else {
    test1 = FALSE
  }
  terms <- labels(terms(lm.fit))
  n.terms <- length(terms)
  if (n.terms < 2) {
    test2 = FALSE
  } else {
    test2 = TRUE
  }
  if (test1 == TRUE & test2 == TRUE) {
    ifelse(
      length(lm.fit$call) > 2,
      new.data <- eval(as.name(lm.fit$call[[3]]),
                       parent.frame()),
      new.data <- model.frame(lm.fit$call[[2]])
    )
    new.scale <- model.matrix(lm.fit)
    v <- as.numeric(row.names(new.scale))
    IV <- attr(lm.fit$terms, "term.labels")
    DV <- dimnames(attr(lm.fit$terms, "factors"))[[1]][1]
    IVx <- dimnames(attr(lm.fit$terms, "factors"))[[1]][-1]
    new.data <- na.omit(new.data[, c(DV, IVx)])
    new.scale[, 1] <- lm.fit$model[, 1]
    new.scale <- data.frame(new.scale)
    colnames(new.scale) <- c(DV, IV)
    beta.out <- coef(lm.fit)[-1] * sapply(new.scale[IV],
                                          "sd") / sapply(new.scale[DV], "sd")
    structure.coef <-
      cor(na.omit(fitted.values(lm.fit)), new.scale[IV])
    tolerances <- 1 / car::vif(lm.fit)
    sqrttol <- tolerances ^ .5
    sr.out <- beta.out * sqrttol
    zeros.out <- cor(new.scale)
    ci <- confint(lm.fit, level = cilevel)
    tvals <- summary(lm.fit)$coefficients[, 3][-1]
    tvals.sq <- tvals ^ 2
    pr_denom <- (tvals.sq + lm.fit$df.residual) ^ .5
    pr.out <- tvals / pr_denom
    CCdata = yhat::commonalityCoefficients(new.data, DV, IV, "F")
    es = yhat::effect.size(lm.fit)
    corrs <-
      as.data.frame(cbind(beta.out, pr.out, sr.out, tolerances, CCdata$CCTotalbyVar))
    names(corrs) <-
      c("beta wt",
        "partial r",
        "semipartial r",
        "tolerances",
        "unique",
        "common",
        "total")
    ifelse (minimal,
            return(corrs),
            return(
              list(
                "Pearson zero-order correlations" = zeros.out,
                "lm summary" = summary(lm.fit),
                "confidence intervals" = ci,
                "Additional IV info" = corrs,
                "Anova Type III SS" = Anova(lm.fit, type = 3),
                "Anova Type I SS" = anova(lm.fit),
                "Effect Size" = es,
                Comment = list(
                  paste0(
                    "The Effect Size recommendations are based on Yin and Fan (2001)."
                  ),
                  paste0("Choose these with care based on your covariance structure")
                )
              )
            ))
  }
  if (test1 == FALSE & test2 == TRUE & minimal == T) {
    return("mrinfo will not process supplemental information for 'no intercept' models")
  }
  if (test1 == FALSE & test2 == TRUE & minimal == F) {
    ci <- confint(lm.fit, level = cilevel)
    return(
      list(
        "CAUTION: interpret a 'no-intercept' model carefully",
        "mrinfo will not process supplemental information for 'no intercept' models",
        "Pearson zero-order correlations" = cor(lm.fit$model),
        "lm summary" = summary(lm.fit),
        "confidence intervals" = ci,
        "Anova Type III SS" = Anova(lm.fit, type = 3),
        "Anova Type I SS" = anova(lm.fit)
      )
    )
  }
  if (test1 == FALSE & test2 == FALSE & minimal == T) {
    return("Supplemental information is not needed for simple regression")
  }
  if (test1 == FALSE & test2 == FALSE & minimal == F) {
    ci <- confint(lm.fit, level = cilevel)
    return(
      list(
        "CAUTION: interpret a 'no-intercept' model carefully",
        "Pearson correlation" = cor(lm.fit$model),
        "lm summary" = summary(lm.fit),
        "confidence intervals" = ci,
        "Anova Type III SS" = Anova(lm.fit, type = 3),
        "Anova Type I SS" = anova(lm.fit)
      )
    )
  }
  if (test1 == TRUE & test2 == FALSE) {
    if (minimal == T) {
      return("Supplemental information is not needed for a simple regression model")
    } else
      if (minimal == F) {
        return(
          list(
            "Pearson correlation" = cor(lm.fit$model),
            "lm summary" = summary(lm.fit),
            "confidence intervals" = confint(lm.fit, level = cilevel),
            "Anova Type III SS" = Anova(lm.fit, type = 3),
            "Anova Type I SS" = anova(lm.fit)
          )
        )
      }
  }
}


