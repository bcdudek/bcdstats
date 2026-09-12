library(rgl)
library(plot3D)
library(plot3Drgl)

library(grid)
library(lattice)
library(ggthemes)
library(shinythemes)
library(ggplot2)
library(plyr)
library(pequod)

means1int <- read.csv("data/multiplicative_categ2x2.csv")

  x <- means1int$x
  y <- means1int$z
  z <- means1int$y
  fit.twocatint5 <- lm(z~x*y)
  # predict values on regular xy grid
  grid.lines = 41
  x.pred <- seq(min(x), max(x), length.out = grid.lines)
  y.pred <- seq(min(y), max(y), length.out = grid.lines)
  xy <- expand.grid( x = x.pred, y = y.pred)
  
  z.pred <- matrix(predict(fit.twocatint5, newdata = xy), 
                   nrow = grid.lines, ncol = grid.lines)
  # fitted points for droplines to surface
  fitpoints <- predict(fit.twocatint5)
  #length(fitpoints)
  
  # scatter plot with multiplicative model regression plane
  # it will NOT be a plane since it is an additive model
  scatter3D(x, y, z, pch = 18, cex = 2, zlim=c(0,50),
            theta = 25, phi = 25, ticktype = "detailed",
            xlab = "Factor A", ylab = "Factor B", zlab = "DV Score",  
            surf = list(x = x.pred, y = y.pred, z = z.pred,  
                        facets = NA, 
                        col="grey75"),
            #fit = fitpoints), 
            colkey=F,col="steelblue4"#,
            #main = "Interaction Surface"
  )
  # note that warped does NOT mean that the fit is nonlinear in either X or Y
  # See that by examining the SME
  
  x0 <- c(-1,-1)
  y0 <- c(-1,1)
  z0 <- c(12,24)
  x1 <- c(1,1)
  y1 <- c(-1,1)
  z1 <- c(16,42)
  x1label <- c(.15,-.45)
  z1label <- c(12.8,38)
  
  Col <- c("blue","blue")
  arrows3D(x0, y0, z0, x1, y1, z1, 
           lwd = 3, lty=1, 
           type="cone", length=.1, col=Col,
           add=T)
  text3D(x1label,y1,z1label, c("A@B1","A@B2"), col="blue", add=T, type="triangle")
  
  plotrgl()
  
  #####  extra code for shiny
  scenetwocatint5 <- scene3d()
  rgl.close()
  save <- options(rgl.inShiny = TRUE)
  on.exit(options(save))
  output$aovint3D5 <- renderRglwidget(rglwidget(scenetwocatint5))
  
  