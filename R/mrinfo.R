#' Regression Information for lm class models
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
#'   \code{structure r} \tab Structure Coefficients\cr
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
#'@details  Structure coefficients can be very helpful in interpretation of
#'\code{lm} models.  The are defined as the pearson correlation of each IV in
#'the model with the yhat vector.  See Thompson and Borello (1985), Cooley and Lohnes
#'(1971), Cohen and Cohen (2003) or Nimon et. al., (2008).
#'
#'@section Warnings:
#'
#'The \code{mrinfo} function is designed to work with multiple regression objects
#'where an intercept is estimated.  Models where the intercept is forced through
#'the origin ('no intercept' models) are problematic for interpretation of the
#'supplemental information listed above and it is not returned.  The specification of
#' \code{minimal=FALSE} will still provide the list of items described above.
#'
#'Simple regression models do not require the supplemental information, but the
#'user can specify \code{minimal=F} to obtain the longer list of detailed information.
#'
#'Models with factor IV's may create situations where interpretation of the
#'supplemental indices is problematic.  This can easily happen with
#'coding schemes such dummy coding (indicator coding or \code{contr.treatment()}).
#'Often, suppressor effect can occur,
#'rendering interpretation of beta weights, partial and semi-partial correlations, and
#'particularly the unique and common variance proportions challenging.  The unique
#'proportion of variance index is calculated as the square of the semi-partial correlation
#'(appropriately).  But the common proportion is calculated as the difference between
#'this unique fraction and the squared zero order pearson correlation for that IV. When
#'suppressor effects arise because of patterns related to contrast coding schemes this
#'common proportion can sometimes be  found as a nonsensical negative quantity. Careful understanding
#'of one's model is required in these circumstances.
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @references This function is a modeled on  a function originally coded
#'    in the \code{regr} function in the \bold{yhat} package.  It uses the
#'    \code{effect.size} function from that package.
#'
#' Cohen, J., & Cohen, J. (2003). *Applied multiple regression/correlation
#' analysis for the behavioral sciences* (3rd ed.). L. Erlbaum Associates.
#'
#' Cooley, W. W., & Lohnes, P. R. (1971). *Multivariate data analysis*. Wiley.
#'
#' Nimon, K., Lewis, M., Kane, R., & Haynes, R. M. (2008). An R package to
#' compute commonality coefficients in the multiple regression case:
#' An introduction to the package and a practical example. *Behavior Research Methods*,
#' 40(2), 457-466.
#'
#' Pedhazur, E. *Multiple regression in behavioral research*. 1997.
#' Orlando, FL: Harcourt.
#'
#' Ray‐Mukherjee, J., Nimon, K., Mukherjee, S., Morris, D. W., Slotow, R.,
#' & Hamer, M. (2014). Using commonality analysis in multiple regressions:
#' a tool to decompose regression effects in the face of multicollinearity.
#' *Methods in Ecology and Evolution*, 5(4), 320-328.
#'
#' Thompson, B., & Borrello, G. M. (1985). The importance of structure coefficients
#' in regression research. *Educational and psychological measurement*, 45(2), 203-209.
#'
#' @examples
#' data(attitude)
#' fit1 <- lm(rating ~ complaints + learning +  privileges, data=attitude)
#' #summary(fit1)
#' mrinfo(fit1, minimal=TRUE, cilevel=.99)
#' mrinfo(fit1, minimal=FALSE, cilevel=.95)
#' data(mtcars)
#' mtcars$cyl <- as.factor(mtcars$cyl)
#' contrasts(mtcars$cyl) <- contr.helmert(3)
#' fit2 <- lm (mpg ~ cyl + hp, data= mtcars)
#' mrinfo(fit2, minimal=TRUE)
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
#' @importFrom HH vif
#' @importFrom car Anova
#'
mrinfo <- function (lm.fit,
                    cilevel = .95,
                    minimal = TRUE)
{
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
  termlist <- attr(lm.fit$terms, "dataClasses")[-1]
  ifelse ("factor" %in% termlist,
          test3 <- TRUE,
          test3 <- FALSE)
  # section1 intercept, MR fac/nofac.  Or int, one IV factor
  ####################################################################################
  if (test1 == TRUE & test2 == TRUE | test1 == TRUE & test2 == FALSE & test3 == TRUE ) {
    new.scale <- model.matrix(lm.fit)
    row.names(new.scale) <- NULL
    IV <- attr(lm.fit$coefficients, "names")
    DV <- dimnames(attr(lm.fit$terms, "factors"))[[1]][1]
    IVx <- IV[-1]
    new.data2 <- cbind.data.frame(lm.fit$model[[1]], new.scale[,-1])
    colnames(new.data2)[1] <- DV # working
    beta.out <- coef(lm.fit)[-1] * sapply(new.data2[IVx],
                                          "sd") / sapply(new.data2[DV], "sd")
    structure.coef <- as.vector(
      cor(na.omit(fitted.values(lm.fit)), new.scale[, IVx]))
    hhvif <- HH::vif(lm.fit)
    tolerances <- 1 / HH::vif(lm.fit)
    sqrttol <- tolerances ^.5
    sr.out <- beta.out * sqrttol
    zeros.out <- cor(new.data2)
    ci <- confint(lm.fit, level = cilevel)
    tvals <- summary(lm.fit)$coefficients[, 3][-1]
    tvals.sq <- tvals ^ 2
    pr_denom <- (tvals.sq + lm.fit$df.residual) ^ .5
    pr.out <- tvals / pr_denom
    unique <- sr.out^2
    common <- zeros.out[DV,IVx]^2 - unique
    total <- unique + common
    es = yhat::effect.size(lm.fit)
    corrs <-
      as.data.frame(cbind(
        round(beta.out, 5),
        round(structure.coef, 5),
        round(pr.out, 5),
        round(sr.out, 5),
        round(tolerances, 5),
        round(unique, 5),
        round(common, 5),
        round(total, 5)))
    names(corrs) <-
      c("beta wt",
        "structure r",
        "partial r",
        "semipartial r",
        "tolerances",
        "unique",
        "common",
        "total")
    ifelse (minimal,
            return(list(
              if (test3 ==TRUE){"In models with factors (categorical IVs), interpretation of supplemental information requires caution"},
              "supplemental information" = corrs,
              "var infl factors from HH:vif"= hhvif,
              if (test3){
                Comment = list(
                  paste0(
                    "For models with factors (categorical IVs), car::vif may be more appropriate"))}
            )),
            return(
              list(
                "Pearson zero-order correlations" = zeros.out,
                "lm summary" = summary(lm.fit),
                "confidence intervals" = ci,
                if (test3 == TRUE){"In models with factors (categorical IVs) interpretation of supplemental information requires caution"},
                "supplemental information" = corrs,
                "var infl factors from HH:vif"= hhvif,
                if (test3 == TRUE){"For models with factors, car::vif may be more appropriate"},
                "Anova Type III SS" = Anova(lm.fit, type = 3),
                "Anova Type I SS" = anova(lm.fit),
                "Effect Size" = es,
                Comment = list(
                  paste0(
                    "Effect sizes from yhat::effect.size();  recommendations based on Yin and Fan (2001)."
                  ),
                  paste0("Choose these with care based on your covariance structure")
                )
              )
            ))
  }
  ####################################################################################
  ## section 2 DONE
  # intercept, simple regression, no factor
  if (test1 == TRUE & test2 == FALSE & test3 == FALSE) {
    new.scale <- model.matrix(lm.fit)
    row.names(new.scale) <- NULL
    IV <- attr(lm.fit$coefficients, "names")
    DV <- dimnames(attr(lm.fit$terms, "factors"))[[1]][1]
    IVx <- IV[-1]
    new.data2 <- cbind.data.frame(lm.fit$model[[1]], new.scale[,-1])
    colnames(new.data2) <- c(DV,IVx) # working
    zeros.out <- cor(new.data2)
    if (minimal == T) {
      return("Supplemental information is not needed for a simple regression model")
    } else
      if (minimal == F) {
        return(
          list(
            "Pearson correlationx" = zeros.out,
            "lm summary" = summary(lm.fit),
            "confidence intervals" = confint(lm.fit, level = cilevel),
            "Anova Type III SS" = Anova(lm.fit, type = 3),
            "Anova Type I SS" = anova(lm.fit)
          )
        )
      }
  }
  ####################################################################################
  ## section3
  ##  zero intercept, MR, without factor
  if (test1 == FALSE & test2 == TRUE & test3 == FALSE & minimal == TRUE) {
    return("mrinfo will not process supplemental information for 'no intercept' models")
  }
  if (test1 == FALSE & test2 == TRUE & test3 == FALSE & minimal == FALSE) {
    new.scale <- model.matrix(lm.fit)
    row.names(new.scale) <- NULL
    IV <- attr(lm.fit$coefficients, "names")
    DV <- dimnames(attr(lm.fit$terms, "factors"))[[1]][1]
    #IVx <- IV[-1]
    new.data2 <- cbind.data.frame(lm.fit$model[[1]], new.scale)
    colnames(new.data2) <-  c(DV,IV) # working
    zeros.out <- cor(new.data2)
    ci <- confint(lm.fit, level = cilevel)
    return(
      list(
        "CAUTION: interpret a 'no-intercept' model carefully",
        "mrinfo will not process supplemental information for 'no intercept' models",
        "Pearson zero-order correlations" = zeros.out,
        "lm summary" = summary(lm.fit),
        "confidence intervals" = ci,
        "Anova Type III SS" = Anova(lm.fit, type = 3),
        "Anova Type I SS" = anova(lm.fit)
      )
    )
  }
  ####################################################################################
  ## section4
  ##  zero intercept, MR, with factor
  if (test1 == FALSE & test2 == TRUE & test3 == TRUE & minimal == TRUE) {
    return("mrinfo will not process supplemental information for 'no intercept' models")
  }
  if (test1 == FALSE & test2 == TRUE & test3 == TRUE & minimal == FALSE) {
    new.scale <- model.matrix(lm.fit)
    row.names(new.scale) <- NULL
    IV <- attr(lm.fit$coefficients, "names")
    DV <- dimnames(attr(lm.fit$terms, "factors"))[[1]][1]
    #IVx <- IV[-1]
    new.data2 <- cbind.data.frame(lm.fit$model[[1]], new.scale)
    colnames(new.data2) <-  c(DV,IV) # working
    #zeros.out <- cor(new.data2)
    ci <- confint(lm.fit, level = cilevel)
    return(
      list(
        "CAUTION: A 'no-intercept' model with a factor IV may be inappropriate",
        "mrinfo will not process supplemental information for 'no intercept' models",
        #"Pearson zero-order correlations" = zeros.out,
        "lm summary" = summary(lm.fit),
        "confidence intervals" = ci,
        "Anova Type III SS" = Anova(lm.fit, type = 3),
        "Anova Type I SS" = anova(lm.fit)
      )
    )
  }
  ####################################################################################
  ## section 5
  # zero intercept, simple regression, no factor
  if (test1 == FALSE & test2 == FALSE & test3 == FALSE & minimal == TRUE) {
    return("supplemental info for zero-intercept models with factors (categorical IVs) is not provided")
  }
  if (test1 == FALSE & test2 == FALSE & test3 == FALSE & minimal == F) {
    new.scale <- model.matrix(lm.fit)
    row.names(new.scale) <- NULL
    IV <- attr(lm.fit$coefficients, "names")
    DV <- dimnames(attr(lm.fit$terms, "factors"))[[1]][1]
    IVx <- IV[-1]
    new.data2 <- cbind.data.frame(lm.fit$model[[1]], new.scale[,1])
    colnames(new.data2) <-  c(DV,IV) # working
    zeros.out <- cor(new.data2)
    ci <- confint(lm.fit, level = cilevel)
    return(
      list(
        "CAUTION: interpret a 'no-intercept' model carefully",
        "Pearson correlation" = zeros.out,
        "lm summary" = summary(lm.fit),
        "confidence intervals" = ci,
        "Anova Type III SS" = Anova(lm.fit, type = 3),
        "Anova Type I SS" = anova(lm.fit)
      )
    )
  }
  ####################################################################################
  ## section 6
  # zero intercept, simple regression, factor IV
  if (test1 == FALSE & test2 == FALSE & test3 == TRUE & minimal == TRUE) {
    return("Supplemental information for no-intercept models with Factor IVs is not provided")
  }
  if (test1 == FALSE & test2 == FALSE & test3 == TRUE & minimal == F) {
    new.scale <- model.matrix(lm.fit)
    row.names(new.scale) <- NULL
    IV <- attr(lm.fit$coefficients, "names")
    DV <- dimnames(attr(lm.fit$terms, "factors"))[[1]][1]
    IVx <- IV[-1]
    new.data2 <- cbind.data.frame(lm.fit$model[[1]], new.scale[,IVx])
    colnames(new.data2) <-  c(DV,IVx)
    zeros.out <- cor(new.data2)
    ci <- confint(lm.fit, level = cilevel)
    return(
      list(
        "CAUTION: A 'no-intercept' model with a factor IV may be inappropriate",
        #"Pearson correlation" = zeros.out,
        "lm summary" = summary(lm.fit),
        "confidence intervals" = ci,
        "Anova Type III SS" = Anova(lm.fit, type = 3),
        "Anova Type I SS" = anova(lm.fit)
      )
    )
  }
}


