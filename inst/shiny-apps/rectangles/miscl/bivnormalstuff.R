# lets first simulate a bivariate normal sample
library(MASS)
bivn <- mvrnorm(1000000, mu = c(0, 0), Sigma = matrix(c(1, .7, .7, 1), 2))

# now we do a kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 5000,h=c(50,50))

# now plot your results

#contour(bivn.kde)
#image(bivn.kde)
win.graph()
persp(bivn.kde, phi = 45, theta = 30)

# fancy contour with image
image(bivn.kde); contour(bivn.kde, add = T)

# fancy perspective
persp(bivn.kde, phi = 45, theta = 30, shade = .1, border = NA)

*****************************************************

require(grDevices) # for colours
x <- -6:16
op <- par(mfrow = c(2, 2))
contour(outer(x, x), method = "edge", vfont = c("sans serif", "plain"))
z <- outer(x, sqrt(abs(x)), FUN = "/")
image(x, x, z)
contour(x, x, z, col = "pink", add = TRUE, method = "edge",
        vfont = c("sans serif", "plain"))
contour(x, x, z, ylim = c(1, 6), method = "simple", labcex = 1,
        xlab = quote(x[1]), ylab = quote(x[2]))
contour(x, x, z, ylim = c(-6, 6), nlev = 20, lty = 2, method = "simple",
        main = "20 levels; \"simple\" labelling method")
par(op)

*****************************************************
  
install.packages("fMultivar",dependencies=T)
library(fMultivar)
## dnorm2d -
# Bivariate Normal Density:
x <- (-40:40)/10
X <- grid2d(x)
z <- dnorm2d(X$x, X$y, rho = 0.65)
ZD <- list(x = x, y = x*100, z = matrix(z, ncol = length(x)))
# Perspective Density Plot:
win.graph()
persp(ZD, theta = -25, phi = 20, col = "skyblue")
# Contour Density Plot:
contour(ZD, main="Bivariate Normal Density")

## pnorm2d -
# Bivariate Normal Probability:
z <- pnorm2d(X$x, X$y, rho = 0.5)
ZP <- list(x = x, y = x, z = matrix(z, ncol = length(x)))
# Perspective Plot:
persp(ZP, theta = -40, phi = 30, col = "steelblue")
# Contour Plot:
contour(ZP)

## rnorm2d - 
# Bivariate Normal Random Deviates
r <- rnorm2d(5000, rho=0.5)
# Scatter Plot:
plot(r, col="steelblue", pch=19, cex=0.5) 
contour(ZD, add=TRUE, lwd=2, col="red")
# Hexagonal Binning:
plot(hexBinning(r))
contour(ZD, add=TRUE, lwd=2, col="black")

  

library(MASS)

# set the correlation you want
my.cor <- 0.9

# Generate a bi-variate normal

my.data <- mvrnorm(n = 1e3, mu = c(0,0), Sigma = matrix(c(1,my.cor,my.cor,1), nrow = 2, byrow = T))

# plot the simulated data
library(ggplot2)

m <- ggplot(as.data.frame(my.data), aes(x = V1, y = V2)) +
  geom_point()
win.graph()
m + geom_density2d()



library(HH)
bv8 <- bivariateNormal(.7)  ## all views on one page
bv8
update(bv8[3], layout=c(1,1)) ## one panel
## Not run: 
shiny::runApp(file.path(system.file(package="HH"), "shiny/bivariateNormal")) ## 3D
shiny::runApp(system.file("shiny/bivariateNormalScatterplot", package="HH")) ## scatterplota


