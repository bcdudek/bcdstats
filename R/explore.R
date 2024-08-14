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
#' @param varname A Variable Label used in the Plot Title
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author Bruce Dudek
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references Howell, D.C. (2010) Statistical Methods for Psychology.  7th Ed.
#' Belmont, CA:Cengage.
#' @keywords ~kwd1 ~kwd2
#' @examples
#'
#' explore(faithful$eruptions, varname="Eruption Duration")
#' explore(mtcars$mpg)
#'
#' @export explore
#' @importFrom grDevices nclass.Sturges
#' @importFrom graphics grid

explore <-
  function(x,varname="", na.rm=T)
  {
require(ggplot2)
require(e1071)
require(qqplotr)
require(grid)
require(gridExtra)
require(psych)
x <- na.omit(x)
### One
set.seed(5)
breaks <- pretty(range(x),
                 n = nclass.Sturges(x),
                 min.n = 1)
p1 <- ggplot(data.frame(x), aes(x))  +
  geom_histogram(aes(y = after_stat(density)),
                 #binwidth = bw,
                 breaks=breaks,
                 colour = "black",fill="skyblue") +
  stat_function(fun = dnorm, args = list(mean = mean(x),
                                         sd = sd(x)),
                color="red", linewidth=.8) +
  ggtitle("Freq Histogram\nwith Normal Distribution") +
  theme_minimal()
p1

### Two
# Data
set.seed(5)
# x <- rnorm(1000)
# df <- data.frame(x)
#data <- read.csv("data/baselinevars.csv")
breaks <- pretty(range(x),
                 n = nclass.Sturges(x),
                 min.n = 1)
# Histogram with kernel density
p2 <- ggplot(data.frame(x), aes(x))  +
  geom_histogram(aes(y =after_stat(density)),
                 #binwidth = bw,
                 breaks=breaks,
                 colour = "black",fill="skyblue") +
  geom_density(lwd = 1, colour = "red",
               fill = 4, alpha = 0.15)+
  ggtitle("Freq Histogram\nwith Kernel Density") +
  theme_minimal()
p2


### Three

p3 <- ggplot(data.frame(x), aes(x=0,y=x)) +
  xlim(-1,1)+
  geom_boxplot(width=.5) + xlab("") +
  geom_point(position = position_jitter(width=.22, height=.2,seed = 135),
             size=1, color="grey") +
  stat_summary(fun=mean, geom="point", shape=23, size=4, fill="dodgerblue") +
  #  geom_jitter(shape=16, width=.2,height=.2) +
  #coord_fixed(.15) + # this won't generalize
  labs(y="y") +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("Boxplot \nwith data and mean")

p3

### Four


gsym.plot <- function (x,
                       N = max(ceiling(1000/length(x)),2),
                       alpha = .05,
                       xlab = "Distance below median",
                       ylab = "Distance above median",
                       main = paste("Symmetry Plot \nskewness =", round(skewness(x), digits=2)),
                       ...) {
  #cat(N, "\n")
  # The symmetry plot
  x <- x[!is.na(x)]
  n <- length(x)
  nn <- ceiling(n/2)
  x <- sort(x)
  d <- as.data.frame(abs(x - median(x)))# Distance to the median
  colnames(d) <- "d"
  dx <- d$d[1:nn]
  dy <- d$d[n:(n-nn+1)]
  diffs <- as.data.frame(cbind(dx,dy))
  # plot( d[1:nn], d[n:(n-nn+1)],
  #       xlab = xlab, ylab = ylab,
  #       main = main,pch=16,
  #       ... )
  # abline(0, 1, col = "blue", lty = 2,lwd=2)
  gg <- ggplot(diffs, aes(x=dx,y=dy)) +
    #geom_jitter(width=.1, height=.1, size=3, seed=3) +
    geom_point(size=2,position = position_jitter(seed=5,
                                                 width=.01,
                                                 height=.01)) +
    geom_abline(slope=1,intercept=0) +
    labs(y=ylab, x=xlab)+
    ggtitle(main)+
    theme_minimal()
  gg
}
p4 <- gsym.plot(x)
p4

### Five
set.seed(5)

p5 <- ggplot(data.frame(x), mapping = aes(sample=x))+
  # geom_qq_band(bandType = "ks", mapping = aes(fill = "KS"), alpha = 0.5) +
  # geom_qq_band(bandType = "ts", mapping = aes(fill = "TS"), alpha = 0.5) +
  # geom_qq_band(bandType = "pointwise", mapping = aes(fill = "Normal"), alpha = 0.5) +
  geom_qq_band(bandType = "boot", distribution="norm",
               alpha = 0.2, fill="dodgerblue") +
  stat_qq_line(distribution="norm") +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  scale_fill_discrete("Bandtype") +
  ggtitle("QQ normal plot \nwith bootstrapped CI") +
  theme_minimal()
p5

### Six
dens <-density(x)
df <- data.frame(x=dens$x, y=dens$y)
probs <- c(0, 0.25, 0.5, 0.75, 1)
quantiles <- quantile(x, prob=probs)
df$quant <- factor(findInterval(df$x,quantiles))
p6 <-
  ggplot(df, aes(x,y)) +
  geom_line() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) +
  scale_x_continuous() +
  #scale_x_continuous(breaks=quantiles) +
  scale_fill_brewer(guide="none")+
  ggtitle("Kernel Density \nwith Quartiles") +
  theme_minimal()
p6

titlename <- paste0("Univariate Plots of ",varname)
#titlename

grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2,
      top = textGrob(titlename,
                 gp=gpar(fontsize=20,font=3)))
psych::describe(x,skew=TRUE, type=2)
  }
#explore2(faithful$eruptions, varname="Eruption Duration")
