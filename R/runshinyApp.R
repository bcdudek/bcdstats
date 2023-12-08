#' runshinyApp, a function to run shiny apps found in bcdstats
#'
#' Several Shiny apps are available in this package.  They are useful
#' as instructional tools for visualizations in an interactive/dynamic
#' framework.  Use the app name as the argument in the \code{runshinyApp} function.
#'
#' @section Probability Distribution Apps: These can be used to find probabilities
#' or to find quantiles.  All apps provide a visulization with appropriate regions of
#' the distribution shown.
#' \itemize{
#' \item
#' **stdnormal**: The standard normal distribution
#' \item
#' **binomial**:  The binomial distribution
#' \item
#' **tdist**: The student's t distribution (central t)
#' \item
#' **chisqdist**:  The Chi squared distribution
#' \item
#' **fdist**:  The F distribution
#' }
#'
#' @section Univariate Descriptive Statistics and Graphical depiction (EDA): Examples of
#' univariate plot types and several data sets.
#' \itemize{
#' \item
#' **describe**: Visualize several types of frequency histograms, boxplots, violinplots, etc
#' }
#'
#' @section Sums of Squares: Visualize the Sums of Squares and Variance calculation
#' \itemize{
#' \item
#' **vizualizess**: A geometric approach to understanding Sums of Squares and Variance
#' }
#'
#' @section Sampling Distribution Simulation: Visualize sampling distributions of several
#' descriptive statistics using differing initial random variable distributions.
#' \itemize{
#' \item
#' **sampdist**: Simulate sampling distributions of several descriptive statistics
#' }
#'
#' @section Visualize distribution overlap and effect sizes for two populations:
#' \itemize{
#' \item
#' **effectsizes_overlap**: Examine overlap indices and visualize normal distribution overlap
#' }
#'
#' @section Visualize P value distributions under various hypotheses:
#' \itemize{
#' \item
#' **pvaluedistribution**: Simulate sampling distributions P values for a one sample test
#' }
#' @section Confidence Interval Visualization: Visualize confidence intervals, "in the long run":
#' \itemize{
#' \item
#' **confidence**: Simulate confidence intervals based on either t or z distributions
#' \item
#' **ci_overlap**: Confidence Interval Overlap and - p values - Inference by eye?
#' }

#' @section Type I and II error visualization: Visualize null and alternative sampling distributions
#' of various characteristics and consequent Type I and II error rates:
#' \itemize{
#' \item
#' **betaprob**: Simulate overlapping null/alternative sampling distributions to visualize Type I
#' and II error rates
#' }
#'
#' @section Correlation/Regression Simulation Apps: Simulate bivariate data and visualize
#' the components of bivariate correlation/covariance and simple regression:
#' \itemize{
#' \item
#' **rectangles**: Visualize the Covariance/SP components
#' \item
#' **corrsim**:  Simulate bivariate correlation and simple regression.  Visualize yhats.
#' }
#'
#' @section Trend Analysis: Visualize components of orthogonal polynomial trend:
#' \itemize{
#' \item
#' **trend**: Simulate application of orthogonal polynomial trend to a one-factor ANOVA design.
#' }
#'
#' @section Interaction and Moderation: Visualize interactions, moderator effects, simple effects:
#' \itemize{
#' \item
#' **mod2**: In two-IV linear models (regression and ANOVA), visualize two-way interactions with
#' simple effects, simple slopes, and regression surfaces.
#' }
#'
#' @param appname The name of the shiny app (IN QUOTES)
#'
#' @author Bruce Dudek <bruce.dudek@@albany.edu>
#'
#' @examples
#'
#'\dontrun{
#' runshinyApp("stdnormal")
#' # to see the list of available apps
#' runshinyApp()
#' }
#' @export runshinyApp
#' @import shiny
#' @importFrom Cairo CairoPDF
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @importFrom stats dnorm
#' @importFrom stats qt
#' @importFrom stats pt
#' @importFrom stats dt
#' @importFrom shinyBS removePopover
#' @importFrom shinyBS addPopover
#' @importFrom shinyBS bsTooltip
#' @importFrom Rmisc summarySE
#' @importFrom ggplot2 ggplot
#' @importFrom dabestr dabest_plot
#' @importFrom dabestr load
#' @importFrom dabestr mean_diff
#' @importFrom dabestr hedges_g
#' @importFrom dabestr cohens_d
#' @importFrom pwr pwr.t.test
#'
runshinyApp <- function(appname){
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-apps", package = "bcdstats"))
  validExamplesMsg <-
  paste0(
    "Valid appnames are: '",
    paste(validExamples, collapse = "',
'"),
    "'")
  # if an invalid example is given, throw an error
  if (missing(appname)|| !nzchar(appname) ||
      !appname %in% validExamples) {
    stop(
      'Please run "runShinyapp()" with a valid app name as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }
  # find and launch the app
  appDir <- system.file("shiny-apps", appname, package = "bcdstats")
  shiny::runApp(appDir, display.mode = "normal")
}
