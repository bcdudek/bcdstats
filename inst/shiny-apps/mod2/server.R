library(shiny)
library(rgl)
library(plot3D)
library(plot3Drgl)
source("packages.R",local=T)
#options(rgl.useNULL = TRUE)

shinyServer(function(input, output, session) {
  #source("data_tools.R",local=T)
  options(rgl.useNULL = TRUE)

  data_summary <- function(data, varname, groupnames){
    require(plyr)
    summary_func <- function(x, col){
      c(mean = mean(x[[col]], na.rm=TRUE),
        sem = sd(x[[col]]/(length(x[[col]]-1)), na.rm=TRUE))
    }
    data_sum<-ddply(data, groupnames, .fun=summary_func,
                    varname)
    data_sum <- rename(data_sum, c("mean" = varname))
    return(data_sum)
  }
  
  # read the raw data set and summarize
  data.addbase <- read.csv("data/additive_categ2x2data.csv")
  myData2 <- data_summary(data.addbase, varname="y", 
                          groupnames=c("factora", "factorb"))
  means1add <- read.csv("data/additive_categ2x2.csv")
  
  data.intbase <- read.csv("data/multiplicative_categ2x2data.csv")
  myData3 <- data_summary(data.intbase, varname="y", 
                          groupnames=c("factora", "factorb"))
  
  
  means1int <- read.csv("data/multiplicative_categ2x2.csv")
  
  baseline <- read.csv("data/baselinevars.csv")
  
  nhanes3b <- read.csv("data/nhanes3b.csv")
  
  observeEvent(
    c(
      input$ivtypes,
      input$twocatplot,
      input$twocatadditive,
      input$twocatadditive3d,
      input$twocatinteraction,
      input$twocatinteraction3d,
      input$onecatplot,
      input$onecatinteraction,
      input$twonumericplot,
      input$twonumericinteraction
    ),
    {
      ###########################################################3
      #twocat additive data, base bar graph, no sme
      if (input$ivtypes == 'twocat' &
          input$twocatplot == 1 &
          input$twocatadditive == 1) {
        output$barsa1 <- renderPlot(
          ggplot(myData2, aes(x = factora, y = y, fill = factorb)) +
            geom_bar(stat = "identity",
                     color = "black",
                     position = position_dodge()) +
            labs(x = "Factor A", y = "Mean DV Score") +
            theme_classic() +
            theme(text = element_text(size = 16)) +
            theme(
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.background = element_blank(),
              axis.line.y = element_line(colour = "black", size = .7),
              axis.line.x = element_line(colour = "black", size = .7),
              plot.title = element_text(hjust = .5),
              legend.title = element_blank()
            ) +
            coord_cartesian(ylim = c(0, 40)) +  scale_y_continuous(expand = c(0, 0)) +
            theme(legend.justification = c(0, 0),
                  legend.position = c(.05, .8)) +
            scale_fill_manual(values = c('slategray4', 'slategray3')) +
            geom_errorbar(aes(ymin = y - sem, ymax = y + sem),
                          width = .2,
                          position = position_dodge(.9))
        )# finish renderplot
      }# finish if for twocat additive data, base bar graph, no sme
      
      #twocat additive data, base bar graph, SME of A and levels of B
      if (input$ivtypes == 'twocat' &
          input$twocatplot == 1 &
          input$twocatadditive == 2) {
        output$barsa2 <- renderPlot(
          ggplot(myData2, aes(x = factora, y = y, fill = factorb)) +
            geom_bar(stat = "identity",
                     color = "black",
                     position = position_dodge()) +
            labs(x = "Factor A", y = "Mean DV Score") +
            theme_classic() +
            theme(text = element_text(size = 16)) +
            theme(
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.background = element_blank(),
              axis.line.y = element_line(colour = "black", size = .7),
              axis.line.x = element_line(colour = "black", size = .7),
              plot.title = element_text(hjust = .5),
              legend.title = element_blank()
            ) +
            coord_cartesian(ylim = c(0, 40)) +  scale_y_continuous(expand = c(0, 0)) +
            theme(legend.justification = c(0, 0),
                  legend.position = c(.05, .8)) +
            scale_fill_manual(values = c('slategray4', 'slategray3')) +
            geom_errorbar(aes(ymin = y - sem, ymax = y + sem),
                          width = .2,
                          position = position_dodge(.9)) +
            geom_segment(x = .75, xend = 1.2, y = 30, yend = 30) +
            geom_segment(x = 1.3, xend = 1.75,y = 30,yend = 30) +
            geom_segment(x = 1.25, xend = 2.25, y = 34, yend = 34) +
            geom_segment(x = .75, xend = .75, y = 30, yend = 13, arrow = arrow() ) +
            geom_segment(x = 1.25, xend = 1.25, y = 34, yend = 25.5, arrow = arrow() ) +
            geom_segment( x = 1.75, xend = 1.75, y = 30, yend = 17, arrow = arrow() ) +
            geom_segment(x = 2.25, xend = 2.25, y = 34, yend = 29,arrow = arrow() )  +
            annotate("text", label = "A @ B1", x = 1, y = 34) +
            annotate("text", label = "= 12 vs 16", x = 1, y = 32) +
            annotate("text", label = "A @ B2", x = 1.75,  y = 38) +
            annotate("text", label = "= 24 vs 28", x = 1.75, y = 36)
            
        )# finish renderplot for additive bars 2
      }# finish if for twocat additive data,  bar graph, sme A at B
      
      #twocat additive data, base bar graph, SME of B at levels of A
      if (input$ivtypes == 'twocat' &
          input$twocatplot == 1 &
          input$twocatadditive == 3) {
        output$barsa3 <- renderPlot(
          ggplot(myData2, aes(
            x = factora, y = y, fill = factorb
          )) +
            geom_bar(
              stat = "identity",
              color = "black",
              position = position_dodge()
            ) +
            labs(x = "Factor A", y = "Mean DV Score") +
            theme_classic() +
            theme(text = element_text(size = 16)) +
            theme(
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.background = element_blank(),
              axis.line.y = element_line(colour = "black", size = .7),
              axis.line.x = element_line(colour = "black", size = .7),
              plot.title = element_text(hjust = .5),
              legend.title = element_blank()
            ) +
            coord_cartesian(ylim = c(0, 40)) +  scale_y_continuous(expand = c(0, 0)) +
            theme(
              legend.justification = c(0, 0),
              legend.position = c(.05, .8)
            ) +
            scale_fill_manual(values = c('slategray4', 'slategray3')) +
            geom_errorbar(
              aes(ymin = y - sem, ymax = y + sem),
              width = .2,
              position = position_dodge(.9)) +
            geom_segment(x=.75,xend=1.25, y=30,yend=30) +
            geom_segment(x=1.75, xend=2.25, y=35,yend=35)+
            geom_segment(x=.75, xend=.75, y=30,yend=13,arrow=arrow()) +
            geom_segment(x=1.25, xend=1.25, y=30,yend=25.5,arrow=arrow()) +
            geom_segment(x=1.75, xend=1.75, y=35,yend=17,arrow=arrow()) +
            geom_segment(x=2.25, xend=2.25, y=35,yend=30,arrow=arrow()) +
            annotate("text",label= "B @ A1", x=1,y=34) +
            annotate("text",label= "= 12 vs 24", x=1,y=32) +
            annotate("text",label= "B @ A2", x=2,y=38.5) +
            annotate("text",label= "= 16 vs 28", x=2,y=36.5)
          
        )# finish renderplot for additive bars 3
      }# finish if for twocat additive data,  bar graph, sme B at A      
 
      #twocat additive data, 3D data only
      if (input$ivtypes == 'twocat' &
          input$twocatplot == 1 &
          input$twocatadditive == 4 &
          input$twocatadditive3d == 1){
        # first, relabel variable names so that var z is the DV for the 3D plots
        #  Note that the 3D plotting approach used here requires variables to be names z 
        #  (the DV), x (the first IV), and y (the second IV), this adds some confusion, 
        #  but the labels on the 3D plot can be specified so that the figure is
        #  interpretable.
        x <- data.addbase$acontrast
        y <- data.addbase$bcontrast
        z <- data.addbase$y
        fit.twocatadd1 <- lm(z~x+y)
        # predict values on regular xy grid
        grid.lines = 51
        x.pred <- seq(min(x), max(x), length.out = grid.lines)
        y.pred <- seq(min(y), max(y), length.out = grid.lines)
        xy <- expand.grid( x = x.pred, y = y.pred)
        z.pred <- matrix(predict(fit.twocatadd1, newdata = xy), 
                         nrow = grid.lines, ncol = grid.lines)
        # fitted points for droplines to surface
        fitpoints <- predict(fit.twocatadd1)
        # scatter plot with regression plane
        scatter3D(x, y, z, pch = 6, cex = 1.2, cex.lab=.5,
                  theta = 25, phi = 5, ticktype = "detailed",  bty="b",
                  xlab = "Factor A", ylab = "Factor B", zlab = "DV Score",  
                  #surf = list(x = x.pred, y = y.pred, z = z.pred,  
                  #            facets = NA, 
                  #            col="grey75"), 
                  #            fit = fitpoints), 
                  colkey=F, col="steelblue4",
                  add=F
                  #main = "2x2 ANOVA Data Scatterplot"
                  )
        plotrgl()
        scenetwocatadd1 <- scene3d()
        rgl.close()
        save <- options(rgl.inShiny = TRUE)
        on.exit(options(save))
        output$aovadd3D1 <- renderRglwidget(rglwidget(scenetwocatadd1))
        
      }# finish twocat additive 3D data only
      
      #twocat additive data, 3D data plus plane
      if (input$ivtypes == 'twocat' &
          input$twocatplot == 1 &
          input$twocatadditive == 4 &
          input$twocatadditive3d == 2){
        # first, relabel variable names so that var z is the DV for the 3D plots
        #  Note that the 3D plotting approach used here requires variables to be names z 
        #  (the DV), x (the first IV), and y (the second IV), this adds some confusion, 
        #  but the labels on the 3D plot can be specified so that the figure is
        #  interpretable.
        x <- data.addbase$acontrast
        y <- data.addbase$bcontrast
        z <- data.addbase$y
        fit.twocatadd2 <- lm(z~x+y)
        # predict values on regular xy grid
        grid.lines = 51
        x.pred <- seq(min(x), max(x), length.out = grid.lines)
        y.pred <- seq(min(y), max(y), length.out = grid.lines)
        xy <- expand.grid( x = x.pred, y = y.pred)
        z.pred <- matrix(predict(fit.twocatadd2, newdata = xy), 
                         nrow = grid.lines, ncol = grid.lines)
        # fitted points for droplines to surface
        fitpoints <- predict(fit.twocatadd2)
        # scatter plot with regression plane
        scatter3D(x, y, z, pch = 6, cex = 1.2, cex.lab=.5,
                  theta = 25, phi = 5, ticktype = "detailed",  bty="b",
                  xlab = "Factor A", ylab = "Factor B", zlab = "DV Score",  
                  surf = list(x = x.pred, y = y.pred, z = z.pred,  
                              facets = NA, 
                              col="grey75"), 
                  #            fit = fitpoints), 
                  colkey=F, col="steelblue4"
                  #main = "2x2 ANOVA Data Scatterplot"
        )
        plotrgl()
        scenetwocatadd2 <- scene3d()
        rgl.close()
        save <- options(rgl.inShiny = TRUE)
        on.exit(options(save))
        output$aovadd3D2 <- renderRglwidget(rglwidget(scenetwocatadd2))
        
      }# finish twocat additive 3D data plus plane      
      
      #twocat additive data, 3D cell means only
      if (input$ivtypes == 'twocat' &
          input$twocatplot == 1 &
          input$twocatadditive == 4 &
          input$twocatadditive3d == 3){
        # first, relabel variable names so that var z is the DV for the 3D plots
        #  Note that the 3D plotting approach used here requires variables to be names z 
        #  (the DV), x (the first IV), and y (the second IV), this adds some confusion, 
        #  but the labels on the 3D plot can be specified so that the figure is
        #  interpretable.
        x <- means1add$x
        y <- means1add$z
        z <- means1add$y
        fit.twocatadd3 <- lm(z~x+y)
        # predict values on regular xy grid
        grid.lines = 51
        x.pred <- seq(min(x), max(x), length.out = grid.lines)
        y.pred <- seq(min(y), max(y), length.out = grid.lines)
        xy <- expand.grid( x = x.pred, y = y.pred)
        z.pred <- matrix(predict(fit.twocatadd3, newdata = xy), 
                         nrow = grid.lines, ncol = grid.lines)
        # fitted points for droplines to surface
        fitpoints <- predict(fit.twocatadd3)
        # scatter plot with regression plane
        scatter3D(x, y, z, pch = 6, cex = 1.4, cex.lab=.5,
                  theta = 25, phi = 5, ticktype = "detailed",  bty="b",
                  xlab = "Factor A", ylab = "Factor B", zlab = "DV Score",  
                  #surf = list(x = x.pred, y = y.pred, z = z.pred,  
                  #            facets = NA, 
                  #            col="grey75"), 
                  #            fit = fitpoints), 
                  colkey=F, col="steelblue4"
                  #main = "2x2 ANOVA Data Scatterplot"
        )
        plotrgl()
        scenetwocatadd3 <- scene3d()
        rgl.close()
        save <- options(rgl.inShiny = TRUE)
        on.exit(options(save))
        output$aovadd3D3 <- renderRglwidget(rglwidget(scenetwocatadd3))
        
      }# finish twocat additive 3D cell means only     
      
      #twocat additive data, 3D cell means plus plane
      if (input$ivtypes == 'twocat' &
          input$twocatplot == 1 &
          input$twocatadditive == 4 &
          input$twocatadditive3d == 4){
        # first, relabel variable names so that var z is the DV for the 3D plots
        #  Note that the 3D plotting approach used here requires variables to be names z 
        #  (the DV), x (the first IV), and y (the second IV), this adds some confusion, 
        #  but the labels on the 3D plot can be specified so that the figure is
        #  interpretable.
        x <- means1add$x
        y <- means1add$z
        z <- means1add$y
        fit.twocatadd4 <- lm(z~x+y)
        # predict values on regular xy grid
        grid.lines = 51
        x.pred <- seq(min(x), max(x), length.out = grid.lines)
        y.pred <- seq(min(y), max(y), length.out = grid.lines)
        xy <- expand.grid( x = x.pred, y = y.pred)
        z.pred <- matrix(predict(fit.twocatadd4, newdata = xy), 
                         nrow = grid.lines, ncol = grid.lines)
        # fitted points for droplines to surface
        fitpoints <- predict(fit.twocatadd4)
        # scatter plot with regression plane
        scatter3D(x, y, z, pch = 21, cex = 1.4, cex.lab=.5,
                  theta = 25, phi = 5, ticktype = "detailed",  bty="b",
                  xlab = "Factor A", ylab = "Factor B", zlab = "DV Score",  
                  surf = list(x = x.pred, y = y.pred, z = z.pred,  
                              facets = NA, 
                              col="grey75"), 
                              fit = fitpoints,
                  colkey=F, col="steelblue4"
                  #main = "2x2 ANOVA Data Scatterplot"
        )
        plotrgl()
        scenetwocatadd4 <- scene3d()
        rgl.close()
        save <- options(rgl.inShiny = TRUE)
        on.exit(options(save))
        output$aovadd3D4 <- renderRglwidget(rglwidget(scenetwocatadd4))
        
      }# finish twocat additive 3D cell means plus plane      
 
      #twocat additive data, 3D cell means plus plane plus sme A
      if (input$ivtypes == 'twocat' &
          input$twocatplot == 1 &
          input$twocatadditive == 4 &
          input$twocatadditive3d == 5){
        # first, relabel variable names so that var z is the DV for the 3D plots
        #  Note that the 3D plotting approach used here requires variables to be names z 
        #  (the DV), x (the first IV), and y (the second IV), this adds some confusion, 
        #  but the labels on the 3D plot can be specified so that the figure is
        #  interpretable.
        x <- means1add$x
        y <- means1add$z
        z <- means1add$y
        fit.twocatadd5 <- lm(z~x+y)
        # predict values on regular xy grid
        grid.lines = 51
        x.pred <- seq(min(x), max(x), length.out = grid.lines)
        y.pred <- seq(min(y), max(y), length.out = grid.lines)
        xy <- expand.grid( x = x.pred, y = y.pred)
        z.pred <- matrix(predict(fit.twocatadd5, newdata = xy), 
                         nrow = grid.lines, ncol = grid.lines)
        # fitted points for droplines to surface
        fitpoints <- predict(fit.twocatadd5)
        # scatter plot with regression plane
        scatter3D(x, y, z, pch = 6, cex = 1.4, cex.lab=.5,
                  theta = 25, phi = 5, ticktype = "detailed",  bty="b",
                  xlab = "Factor A", ylab = "Factor B", zlab = "DV Score",  
                  surf = list(x = x.pred, y = y.pred, z = z.pred,  
                              facets = NA, 
                              col="grey75"),
                  #fit = fitpoints), 
        colkey=F, col="steelblue4"
        #main = "2x2 ANOVA Data Scatterplot"
         )
  
  #Add Arrows for SME
  
  x0 <- c(-1,-1)
  y0 <- c(-1,1)
  z0 <- c(12,24)
  x1 <- c(1,1)
  y1 <- c(-1,1)
  z1 <- c(16,28)
  x1label <- c(-.05,-.35)
  z1label <- c(12.5,27)
  
  Col <- c("blue","blue")
  arrows3D(x0, y0, z0, x1, y1, z1, 
           lwd = 3, lty=1, 
           type="simple", length=1, col=Col,
           add=T)
  text3D(x1label,y1,z1label, c("A@B1","A@B2"), col="blue", add=T)
  
  plotrgl()
  scenetwocatadd5 <- scene3d()
  rgl.close()
  save <- options(rgl.inShiny = TRUE)
  on.exit(options(save))
  output$aovadd3D5 <- renderRglwidget(rglwidget(scenetwocatadd5))
  
      }# finish twocat additive 3D cell means plus plane plus SME A      
           
      
      
      
      ###########################################################3
      #twocat interaction data, base bar graph, no sme
      if (input$ivtypes == 'twocat' &
          input$twocatplot == 2 &
          input$twocatinteraction == 1) {
        output$barsi1 <- renderPlot(
          ggplot(myData3, aes(x = factora, y = y, fill = factorb)) +
            geom_bar(stat = "identity",
                     color = "black",
                     position = position_dodge()) +
            labs(x = "Factor A", y = "Mean DV Score") +
            theme_classic() +
            theme(text = element_text(size = 16)) +
            theme(
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.background = element_blank(),
              axis.line.y = element_line(colour = "black", size = .7),
              axis.line.x = element_line(colour = "black", size = .7),
              plot.title = element_text(hjust = .5),
              legend.title = element_blank()
            ) +
            coord_cartesian(ylim = c(0, 60)) +  scale_y_continuous(expand = c(0, 0)) +
            theme(legend.justification = c(0, 0),
                  legend.position = c(.05, .8)) +
            scale_fill_manual(values = c('slategray4', 'slategray3')) +
            geom_errorbar(aes(ymin = y - sem, ymax = y + sem),
                          width = .2,
                          position = position_dodge(.9))
        )# finish renderplot
      }# finish if for twocat interaction data, base bar graph, no sme``      
      
     #twocat interaction data, base bar graph, SME of A and levels of B
      if (input$ivtypes == 'twocat' &
          input$twocatplot == 2 &
          input$twocatinteraction == 2) {
        output$barsi2 <- renderPlot(
          ggplot(myData3, aes(x = factora, y = y, fill = factorb)) +
            geom_bar(stat = "identity",
                     color = "black",
                     position = position_dodge()) +
            labs(x = "Factor A", y = "Mean DV Score") +
            theme_classic() +
            theme(text = element_text(size = 16)) +
            theme(
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.background = element_blank(),
              axis.line.y = element_line(colour = "black", size = .7),
              axis.line.x = element_line(colour = "black", size = .7),
              plot.title = element_text(hjust = .5),
              legend.title = element_blank()
            ) +
            coord_cartesian(ylim = c(0, 60)) +  scale_y_continuous(expand = c(0, 0)) +
            theme(legend.justification = c(0, 0),
                  legend.position = c(.05, .8)) +
            scale_fill_manual(values = c('slategray4', 'slategray3')) +
            geom_errorbar(
              aes(ymin = y - sem, ymax = y + sem),
              width = .2,
              position = position_dodge(.9)) +
            geom_segment(x=.75,xend=1.2, y=30,yend=30) +
            geom_segment(x=1.3,xend=1.75, y=30,yend=30) +
            geom_segment(x=1.25, xend=2.25, y=52,yend=52)+
            geom_segment(x=.75, xend=.75, y=30,yend=13,arrow=arrow()) +
            geom_segment(x=1.25, xend=1.25, y=52,yend=25.5,arrow=arrow()) +
            geom_segment(x=1.75, xend=1.75, y=30,yend=17,arrow=arrow()) +
            geom_segment(x=2.25, xend=2.25, y=52,yend=43,arrow=arrow()) + 
            annotate("text",label= "A @ B1", x=1,y=36) +
            annotate("text",label= "= 12 vs 16", x=1,y=33) +
            annotate("text",label= "A @ B2", x=1.75,y=58) +
            annotate("text",label= "= 24 vs 42", x=1.75,y=55)
          
        )# finish renderplot for interaction bars 2
      }# finish if for twocat interaction data,  bar graph, sme A at B      
      
      #twocat interaction data, base bar graph, SME of B at levels of A
      if (input$ivtypes == 'twocat' &
          input$twocatplot == 2 &
          input$twocatinteraction == 3) {
        output$barsi3 <- renderPlot(
          ggplot(myData3, aes(
            x = factora, y = y, fill = factorb
          )) +
            geom_bar(
              stat = "identity",
              color = "black",
              position = position_dodge()
            ) +
            labs(x = "Factor A", y = "Mean DV Score") +
            theme_classic() +
            theme(text = element_text(size = 16)) +
            theme(
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.background = element_blank(),
              axis.line.y = element_line(colour = "black", size = .7),
              axis.line.x = element_line(colour = "black", size = .7),
              plot.title = element_text(hjust = .5),
              legend.title = element_blank()
            ) +
            coord_cartesian(ylim = c(0, 60)) +  scale_y_continuous(expand = c(0, 0)) +
            theme(
              legend.justification = c(0, 0),
              legend.position = c(.05, .8)
            ) +
            scale_fill_manual(values = c('slategray4', 'slategray3')) +
            geom_errorbar(
              aes(ymin = y - sem, ymax = y + sem),
              width = .2,
              position = position_dodge(.9)) +
            geom_segment(x=.75,xend=1.25, y=32,yend=32) +
            geom_segment(x=1.75, xend=2.25, y=52,yend=52)+
            geom_segment(x=.75, xend=.75, y=32,yend=13,arrow=arrow()) +
            geom_segment(x=1.25, xend=1.25, y=32,yend=25.5,arrow=arrow()) +
            geom_segment(x=1.75, xend=1.75, y=52,yend=17,arrow=arrow()) +
            geom_segment(x=2.25, xend=2.25, y=52,yend=43,arrow=arrow()) +  
            annotate("text",label= "B @ A1", x=1,y=38) +
            annotate("text",label= "= 12 vs 24", x=1,y=35) +
            annotate("text",label= "B @ A2", x=2,y=58) +
            annotate("text",label= "= 16 vs 42", x=2,y=55)
          
        )# finish renderplot for interaction bars 3
      }# finish if for twocat interaction data,  bar graph, sme B at A      
 
      #twocat interaction data, 3D data only
      if (input$ivtypes == 'twocat' &
          input$twocatplot == 2 &
          input$twocatinteraction == 4 &
          input$twocatinteraction3d == 1){
        # first, relabel variable names so that var z is the DV for the 3D plots
        #  Note that the 3D plotting approach used here requires variables to be names z 
        #  (the DV), x (the first IV), and y (the second IV), this adds some confusion, 
        #  but the labels on the 3D plot can be specified so that the figure is
        #  interpretable.
        x <- data.intbase$acontrast
        y <- data.intbase$bcontrast
        z <- data.intbase$y
        fit.twocatint1<- lm(z~x+y)
        # predict values on regular xy grid
        grid.lines = 51
        x.pred <- seq(min(x), max(x), length.out = grid.lines)
        y.pred <- seq(min(y), max(y), length.out = grid.lines)
        xy <- expand.grid( x = x.pred, y = y.pred)
        z.pred <- matrix(predict(fit.twocatint1, newdata = xy), 
                         nrow = grid.lines, ncol = grid.lines)
        # fitted points for droplines to surface
        fitpoints <- predict(fit.twocatint1)
        # scatter plot with regression plane
        scatter3D(x, y, z, pch = 6, cex = 1.2, cex.lab=.5,
                  theta = 25, phi = 5, ticktype = "detailed",  bty="b",
                  xlab = "Factor A", ylab = "Factor B", zlab = "DV Score",  
                  #surf = list(x = x.pred, y = y.pred, z = z.pred,  
                  #            facets = NA, 
                  #            col="grey75"), 
                  #            fit = fitpoints), 
                  colkey=F, col="steelblue4",
                  add=F
                  #main = "2x2 ANOVA Data Scatterplot"
        )
        plotrgl()
        scenetwocatint1 <- scene3d()
        rgl.close()
        save <- options(rgl.inShiny = TRUE)
        on.exit(options(save))
        output$aovint3D1 <- renderRglwidget(rglwidget(scenetwocatint1))
        
      }# finish twocat interaction 3D data only
 
      #twocat interaction data, 3D additive model on data
      if (input$ivtypes == 'twocat' &
          input$twocatplot == 2 &
          input$twocatinteraction == 4 &
          input$twocatinteraction3d == 2){
        # first, relabel variable names so that var z is the DV for the 3D plots
        #  Note that the 3D plotting approach used here requires variables to be names z 
        #  (the DV), x (the first IV), and y (the second IV), this adds some confusion, 
        #  but the labels on the 3D plot can be specified so that the figure is
        #  interpretable.
        x <- data.intbase$acontrast
        y <- data.intbase$bcontrast
        z <- data.intbase$y
        fit.twocatint2 <- lm(z~x+y)
        # predict values on regular xy grid
        grid.lines = 51
        x.pred <- seq(min(x), max(x), length.out = grid.lines)
        y.pred <- seq(min(y), max(y), length.out = grid.lines)
        xy <- expand.grid( x = x.pred, y = y.pred)
        z.pred <- matrix(predict(fit.twocatint2, newdata = xy), 
                         nrow = grid.lines, ncol = grid.lines)
        # fitted points for droplines to surface
        fitpoints <- predict(fit.twocatint2)
        # scatter plot with regression plane
        scatter3D(x, y, z, pch = 6, cex = 1.2, cex.lab=.5,
                  theta = 25, phi = 5, ticktype = "detailed",  bty="b",
                  xlab = "Factor A", ylab = "Factor B", zlab = "DV Score",  
                  surf = list(x = x.pred, y = y.pred, z = z.pred,  
                              facets = NA, 
                              col="grey75"), 
                  #fit = fitpoints), 
                  colkey=F, col="steelblue4"
                  #main = "2x2 ANOVA Data Scatterplot"
        )
        plotrgl()
        scenetwocatint2 <- scene3d()
        rgl.close()
        save <- options(rgl.inShiny = TRUE)
        on.exit(options(save))
        output$aovint3D2 <- renderRglwidget(rglwidget(scenetwocatint2))
  
      }# finish twocat interaction 3D data plus additive surface     
           

      #twocat interaction MEANS, 3D additive model on 
      if (input$ivtypes == 'twocat' &
          input$twocatplot == 2 &
          input$twocatinteraction == 4 &
          input$twocatinteraction3d == 3){
        # first, relabel variable names so that var z is the DV for the 3D plots
        #  Note that the 3D plotting approach used here requires variables to be names z 
        #  (the DV), x (the first IV), and y (the second IV), this adds some confusion, 
        #  but the labels on the 3D plot can be specified so that the figure is
        #  interpretable.
        x <- means1int$x
        y <- means1int$z
        z <- means1int$y
        fit.twocatint3 <- lm(z~x+y)
        # predict values on regular xy grid
        grid.lines = 41
        x.pred <- seq(min(x), max(x), length.out = grid.lines)
        y.pred <- seq(min(y), max(y), length.out = grid.lines)
        xy <- expand.grid( x = x.pred, y = y.pred)
        
        z.pred <- matrix(predict(fit.twocatint3, newdata = xy), 
                         nrow = grid.lines, ncol = grid.lines)
        # fitted points for droplines to surface
        fitpoints <- predict(fit.twocatint3)
        #length(fitpoints)
        
        # scatter plot with additive model regression plane
        # it will be a plane since it is an additive model
        scatter3D(x, y, z, pch = 18, cex = 2, zlim=c(0,50),
                  theta = 25, phi = 25, ticktype = "detailed",
                  xlab = "Factor A", ylab = "Factor B", zlab = "DV Score",  
                  surf = list(x = x.pred, y = y.pred, z = z.pred,  
                              facets = NA, 
                              col="grey75",
                              fit = fitpoints), 
                  colkey=F,col="steelblue4"#,
                  #main = "Additive model is a poor fit to the means"
        )
        plotrgl()
        scenetwocatint3 <- scene3d()
        rgl.close()
        save <- options(rgl.inShiny = TRUE)
        on.exit(options(save))
        output$aovint3D3 <- renderRglwidget(rglwidget(scenetwocatint3))
        
      }# finish twocat interaction 3D means plus additive surface  
      
      #twocat interaction MEANS, 3D interaction model 
      if (input$ivtypes == 'twocat' &
          input$twocatplot == 2 &
          input$twocatinteraction == 4 &
          input$twocatinteraction3d == 4){
        # first, relabel variable names so that var z is the DV for the 3D plots
        #  Note that the 3D plotting approach used here requires variables to be names z 
        #  (the DV), x (the first IV), and y (the second IV), this adds some confusion, 
        #  but the labels on the 3D plot can be specified so that the figure is
        #  interpretable.
        x <- means1int$x
        y <- means1int$z
        z <- means1int$y
        fit.twocatint4 <- lm(z~x*y)
        # predict values on regular xy grid
        grid.lines = 41
        x.pred <- seq(min(x), max(x), length.out = grid.lines)
        y.pred <- seq(min(y), max(y), length.out = grid.lines)
        xy <- expand.grid( x = x.pred, y = y.pred)
        
        z.pred <- matrix(predict(fit.twocatint4, newdata = xy), 
                         nrow = grid.lines, ncol = grid.lines)
        # fitted points for droplines to surface
        fitpoints <- predict(fit.twocatint4)
        #length(fitpoints)
        
        # scatter plot with additive model regression plane
        # it will be a plane since it is an additive model
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

                  plotrgl()
                  scenetwocatint4 <- scene3d()
                  rgl.close()
                  save <- options(rgl.inShiny = TRUE)
                  on.exit(options(save))
                  output$aovint3D4 <- renderRglwidget(rglwidget(scenetwocatint4))
                  
      }# finish twocat interaction 3D means plus interaction surface  
      
      #twocat interaction MEANS, 3D interaction model plus sme of A
      if (input$ivtypes == 'twocat' &
          input$twocatplot == 2 &
          input$twocatinteraction == 4 &
          input$twocatinteraction3d == 5){
        # first, relabel variable names so that var z is the DV for the 3D plots
        #  Note that the 3D plotting approach used here requires variables to be names z 
        #  (the DV), x (the first IV), and y (the second IV), this adds some confusion, 
        #  but the labels on the 3D plot can be specified so that the figure is
        #  interpretable.
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
        
        # scatter plot with additive model regression plane
        # it will be a plane since it is an additive model
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
                 type="simple", length=.01, col=Col,
                 add=T)
        text3D(x1label,y1,z1label, c("A@B1","A@B2"), col="blue", add=T)
        
        plotrgl()
        scenetwocatint5 <- scene3d()
        rgl.close()
        save <- options(rgl.inShiny = TRUE)
        on.exit(options(save))
        output$aovint3D5 <- renderRglwidget(rglwidget(scenetwocatint5))
        
      }# finish twocat interaction 3D means plus interaction surface plus SME of A      
      
      ###########################################################
      #onecat data set: nhanes
      
      # first, a traditional bivariate scatterplot
      if (input$ivtypes == 'onecat' &
          input$onecatplot == 1){
     output$onecatbivariate <- renderPlot(
        ggplot(nhanes3b,
               aes(x = bmpwtlbs, y = systolic, color = smoker)) +
          ylab("Systolic Blood Pressure") +
          xlab("Body Weight (Lbs)") +
          geom_point(size = 1.6, colour = "black",shape=1) + 
          geom_point(size = 1.2) + # 
          scale_color_manual(values=c("skyblue2","coral1"),
                             labels=c("Non-smoker","Smoker"))+
          #  scale_color_grey(start = 0.8, end = 0.2) +
          theme_few()+
          theme(legend.position=c(.82,.85))+
          theme(legend.title=element_blank())+
          stat_smooth(method="lm",aes(colour=factor(smoker))) 
       ) # finish renderplot for biv scatterplot
      }#finish onecatplot ifs for biv scatterplot
          
      # one cat 3d plot data only
      if (input$ivtypes == 'onecat' &
          input$onecatplot == 2){
        # now the 3D surface plot additive model
        z <- nhanes3b$systolic
        y <- nhanes3b$bmpwtlbs
        x <- nhanes3b$smoker2
        
        fit.nh3add1 <- lm(z~x+y)
        # set up a grid required for the plane drawn by the surface argument of scatter3D
        grid.lines = 31
        x.pred <- seq(min(x), max(x), length.out = grid.lines)
        y.pred <- seq(min(y), max(y), length.out = grid.lines)
        xy <- expand.grid( x = x.pred, y = y.pred)
        z.pred <- matrix(predict(fit.nh3add1, newdata = xy), 
                         nrow = grid.lines, ncol = grid.lines)
        # fitted points for droplines to surface
        fitpoints <- predict(fit.nh3add1)
        # scatter plot with regression plane
        scatter3D(x, y, z, pch = 18, cex = 1.1, 
                  theta = 20, phi =10, ticktype = "detailed",
                  #cex.axis=.9,
                  xlab = "Smoker", ylab = "Body Wt", zlab = "Systolic BP",  
                  #surf = list(x = x.pred, y = y.pred, z = z.pred,  
                  #            facets = NA, 
                  #            col="grey75", 
                  #            fit = fitpoints), 
                  colkey=F, col=heat.colors(length(z)))
                  #main = "Two Predictor Model\n Data Only")
        plotrgl()
        sceneonecat3d1 <- scene3d()
        rgl.close()
        save <- options(rgl.inShiny = TRUE)
        on.exit(options(save))
        output$onecat3d1 <- renderRglwidget(rglwidget(sceneonecat3d1))
        
      }#finish onecatplot ifs for 3d data only
      
      # one cat 3d plot data plus additive plane
      if (input$ivtypes == 'onecat' &
          input$onecatplot == 3){
        # now the 3D surface plot additive model
        z <- nhanes3b$systolic
        y <- nhanes3b$bmpwtlbs
        x <- nhanes3b$smoker2
        
        fit.nh3add2 <- lm(z~x+y)
        # set up a grid required for the plane drawn by the surface argument of scatter3D
        grid.lines = 31
        x.pred <- seq(min(x), max(x), length.out = grid.lines)
        y.pred <- seq(min(y), max(y), length.out = grid.lines)
        xy <- expand.grid( x = x.pred, y = y.pred)
        z.pred <- matrix(predict(fit.nh3add2, newdata = xy), 
                         nrow = grid.lines, ncol = grid.lines)
        # fitted points for droplines to surface
        fitpoints <- predict(fit.nh3add2)
        # scatter plot with regression plane
        scatter3D(x, y, z, pch = 18, cex = .7, 
                  theta = 20, phi =10, ticktype = "detailed",
                  #cex.axis=.9,
                  xlab = "Smoker", ylab = "Body Wt", zlab = "Systolic BP",  
                  surf = list(x = x.pred, y = y.pred, z = z.pred,  
                              facets = NA, 
                              col="grey75"),
                  #            fit = fitpoints), 
                  colkey=F, 
                  col=heat.colors(length(z)))
                  #main = "Two Predictor Model\n Additive model surface - plane")
        plotrgl()
        sceneonecat3d2 <- scene3d()
        rgl.close()
        save <- options(rgl.inShiny = TRUE)
        on.exit(options(save))
        output$onecat3d2 <- renderRglwidget(rglwidget(sceneonecat3d2))
        
      }#finish onecatplot ifs for 3d data plus additive plane
      
      # one cat 3d plot data plus interaction plane
      if (input$ivtypes == 'onecat' &
          input$onecatplot == 4 &
          input$onecatinteraction == 1){
        # now the 3D surface plot interaction model
        z <- nhanes3b$systolic
        y <- nhanes3b$bmpwtlbs
        x <- nhanes3b$smoker2
        
        fit.nh3int1 <- lm(z~x*y)
        # set up a grid required for the plane drawn by the surface argument of scatter3D
        grid.lines = 31
        x.pred <- seq(min(x), max(x), length.out = grid.lines)
        y.pred <- seq(min(y), max(y), length.out = grid.lines)
        xy <- expand.grid( x = x.pred, y = y.pred)
        z.pred <- matrix(predict(fit.nh3int1, newdata = xy), 
                         nrow = grid.lines, ncol = grid.lines)
        # fitted points for droplines to surface
        fitpoints <- predict(fit.nh3int1)
        # scatter plot with regression plane
        scatter3D(x, y, z, pch = 18, cex = .7, 
                  theta = 20, phi =10, ticktype = "detailed",
                  #cex.axis=.9,
                  xlab = "Smoker", ylab = "Body Wt", zlab = "Systolic BP",  
                  surf = list(x = x.pred, y = y.pred, z = z.pred,  
                              facets = NA, 
                              col="grey75"),
                  #            fit = fitpoints), 
                  colkey=F,  col=heat.colors(length(z)),
                  main = "Interaction surface - warped plane")
        plotrgl()
        sceneonecat3d3 <- scene3d()
        rgl.close()
        save <- options(rgl.inShiny = TRUE)
        on.exit(options(save))
        output$onecat3d3 <- renderRglwidget(rglwidget(sceneonecat3d3))
        
      }#finish onecatplot ifs for 3d data plus warped plane          
          
      
      # one cat 3d plot data plus interaction plane plus simple slopes
      if (input$ivtypes == 'onecat' &
          input$onecatplot == 4 &
          input$onecatinteraction == 2
          ){
        # now the 3D surface plot interaction model
        nhanes3b$cbw <- nhanes3b$bmpwtlbs- mean(nhanes3b$bmpwtlbs)
        peqmod2 <- lmres(systolic ~ smoker2+bmpwtlbs+smoker2:bmpwtlbs, 
                         center="bmpwtlbs", data=nhanes3b)
        
        # now obtain simple slopes analysis
        # first for model2
        # recall that for the smoker2 variable, a 1 codes smoker and a -1 codes nonsmoker
        s_slopes2<-simpleSlope(peqmod2,pred="bmpwtlbs", mod1="smoker2")
        
        
        # now extract slopes and intercepts for passage to 3D plot
        # have to pull the two intercepts by algebra from the base lmres model info
        # the simple slopes can come from the s_slopes object
        smokerslope2.int <- peqmod2$Stepfin$coefficients["(Intercept)"] +
          peqmod2$Stepfin$coefficients["smoker2"] # the +1 SD here
        smokerslope2.slope <- s_slopes2$simple_slope[2]# the +1 SD here
        nonsmokerslope2.int <- peqmod2$Stepfin$coefficients["(Intercept)"] -
          peqmod2$Stepfin$coefficients["smoker2"] # the -1 SD here
        nonsmokerslope2.slope <- s_slopes2$simple_slope[1] # the -1 SD here
        
        
        bwlow <- min(nhanes3b$cbw)
        bwhigh <- max(nhanes3b$cbw)
        
        slow <- peqmod2$Stepfin$coefficients["(Intercept)"]+
          peqmod2$Stepfin$coefficients[2]* 1 +
          peqmod2$Stepfin$coefficients[3]*min(nhanes3b$cbw)+
          peqmod2$Stepfin$coefficients[4]*(min(nhanes3b$cbw)*1)
        
        shigh <- peqmod2$Stepfin$coefficients["(Intercept)"]+
          peqmod2$Stepfin$coefficients[2]* 1 +
          peqmod2$Stepfin$coefficients[3]*max(nhanes3b$cbw)+
          peqmod2$Stepfin$coefficients[4]*(max(nhanes3b$cbw)*1)
        
        nslow <- peqmod2$Stepfin$coefficients["(Intercept)"]+
          peqmod2$Stepfin$coefficients[2]*-1 +
          peqmod2$Stepfin$coefficients[3]*min(nhanes3b$cbw)+
          peqmod2$Stepfin$coefficients[4]*(min(nhanes3b$cbw)*-1)
        
        nshigh <- peqmod2$Stepfin$coefficients["(Intercept)"]+
          peqmod2$Stepfin$coefficients[2]* -1 +
          peqmod2$Stepfin$coefficients[3]*max(nhanes3b$cbw)+
          peqmod2$Stepfin$coefficients[4]*(max(nhanes3b$cbw)*-1)
        
        # now we can finally do the 3D plot with the simple slopes shown
        # initially recreate the base 3D plot
        # scatter plot with regression surface using centered bw
        z <- nhanes3b$systolic
        y <- nhanes3b$cbw
        x <- nhanes3b$smoker2
        
        peqmod2b <- lm(z~x*y)
        summary(peqmod2b)
        summary(peqmod2)
        # set up a grid required for the plane drawn by the surface argument of scatter3D
        grid.lines = 31
        x.pred <- seq(min(x), max(x), length.out = grid.lines)
        y.pred <- seq(min(y), max(y), length.out = grid.lines)
        xy <- expand.grid( x = x.pred, y = y.pred)
        
        z.pred <- matrix(predict(peqmod2b, newdata = xy), 
                         nrow = grid.lines, ncol = grid.lines)
        # fitted points for droplines to surface
        fitpoints <- predict(peqmod2b)
        
        
        ## @knitr nh3pequod7c
        scatter3D(x, y, z, pch = 18, cex = .8, 
                  theta = 30, phi =5, ticktype = "detailed",
                  #cex.axis=.9,
                  xlab = "Smoker", ylab = "BW (centered)", zlab = "Systolic BP",  
                  surf = list(x = x.pred, y = y.pred, z = z.pred,  
                              facets = NA, 
                              col="grey75", 
                              fit = NULL), 
                  colkey=F, col="grey90",
                  bty="u"#,
                  #main = "Two Predictor\n Interaction Model"
        )
        #Add Arrows(lines) for SME
        
        x0 <- c(-1,1)
        y0 <- c(bwlow,bwlow)
        z0 <- c(nslow,slow)
        x1 <- c(-1,1)
        y1 <- c(bwhigh,bwhigh)
        z1 <- c(nshigh,shigh)
        
        
        Col <- c("blue","blue")
        arrows3D(x0, y0, z0, x1, y1, z1, 
                 lwd = 7, lty=1, 
                 type="simple", length=.01, col=Col,
                 add=T)
        
        x1label <- c(-.75,.45)
        z1label <- c(163,95)
        y1label <- c(-75,-81)
        text3D(x1label,y1label,z1label, c("Y~BW @ NonSmoker",
                                          "Y~BW @ Smoker"), col="black", add=T)
        #text2D(-1,0,140, "systolic on BW at Smoker", col="blue", add=T)
        
        # add four points
        xpt <- c(-1,-1,1,1)
        ypt <- c(-sd(nhanes3b$cbw),sd(nhanes3b$cbw),-sd(nhanes3b$cbw),sd(nhanes3b$cbw))
        zpt <- c(s_slopes2$Points[1],
                 s_slopes2$Points[3],
                 s_slopes2$Points[2],
                 s_slopes2$Points[4])
        col2 <- c("skyblue","skyblue","dodgerblue","dodgerblue")
        points3D(xpt,ypt,zpt,add=T,cex=2,pch=22, 
                 col="black",bg=col2,colkey=F)
        
        plotrgl()
        sceneonecat3d4 <- scene3d()
        rgl.close()
        save <- options(rgl.inShiny = TRUE)
        on.exit(options(save))
        output$onecat3d4 <- renderRglwidget(rglwidget(sceneonecat3d4))
        
      }#finish onecatplot ifs for 3d data plus warped plane and simple slopes         
      
      
      
      
      
                      
      ###########################################################3
      #twonumeric data 
      
      if (input$ivtypes == 'zerocat' &
          input$twonumericplot == 1) {
        z <- baseline$bods
        x <- baseline$boasev
        y <- baseline$bpa
        #options(rgl.useNULL = TRUE)``
        scatter3D(
          x = x,
          y = y,
          z = z,
          pch = 21, cex = .6, cex.lab = .7, bty = "b2",
          theta = 25,  phi = -2,
          ticktype = "detailed",
          zlim = c(-4, 21.5),
          xlab = "Anxiety Severity",
          ylab = "Positive Affect",
          zlab = "Depression Severity",
          #surf = list(x = x.pred, y = y.pred, z = z.pred,
          #            facets = NA,
          #            col="grey75",
          #            fit = fitpoints),
          colkey = F,
          col = "steelblue4",
          main = "",
          plot = T
        )
        plotrgl()
        scenetwonumericdata <- scene3d()
        rgl.close()
        save <- options(rgl.inShiny = TRUE)
        on.exit(options(save))
        output$twonumeric1 <- renderRglwidget(rglwidget(scenetwonumericdata))
        #)# finish renderplot for twonumericplane
      }# finish ifs for twonumeric data 
      
      
      ###########################################################3
      #twonumeric data plus additive surface
      
      if (input$ivtypes == 'zerocat' &
               input$twonumericplot == 2) {

        z <- baseline$bods
        x <- baseline$boasev
        y <- baseline$bpa
        fit.dep1b <- lm(z~x+y)

        # set up a grid required for the plane drawn by the surface argument of scatter3D
        grid.lines = 21
        x.pred <- seq(min(x), max(x), length.out = grid.lines)
        y.pred <- seq(min(y), max(y), length.out = grid.lines)
        xy <- expand.grid( x = x.pred, y = y.pred)
        
        z.pred <- matrix(predict(fit.dep1b, newdata = xy), 
                         nrow = grid.lines, ncol = grid.lines)
        # fitted points for droplines to surface
        fitpoints <- predict(fit.dep1b)
        # scatter plot with regression plane
        scatter3D(
          x = x,
          y = y,
          z = z,
          pch = 21, cex = .4, cex.lab = .7, bty = "b2",
          theta = 25,  phi = -2,
          ticktype = "detailed",
          zlim = c(-4, 21.5),
          xlab = "Anxiety Severity",
          ylab = "Positive Affect",
          zlab = "Depression Severity",
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = NA,
                      col="grey75"),
                      #fit = fitpoints),
          colkey = F,
          col = "steelblue4",
          main = "",
          plot = T
        )
        plotrgl()
        scenetwonumericplane <- scene3d()
        rgl.close()
        save <- options(rgl.inShiny = TRUE)
        on.exit(options(save))
        output$twonumeric2 <- renderRglwidget(rglwidget(scenetwonumericplane))

      }# finish ifs for twonumeric additive surface 3D plot
 
      ###########################################################3
      #twonumeric data plus interaction surface
      
      if (input$ivtypes == 'zerocat' &
               input$twonumericplot == 3 &
               input$twonumericinteraction == 1){
        
        z <- baseline$bods
        x <- baseline$boasev
        y <- baseline$bpa
        fit.dep1b <- lm(z~x*y)
        
        # set up a grid required for the plane drawn by the surface argument of scatter3D
        grid.lines = 21
        x.pred <- seq(min(x), max(x), length.out = grid.lines)
        y.pred <- seq(min(y), max(y), length.out = grid.lines)
        xy <- expand.grid( x = x.pred, y = y.pred)
        
        z.pred <- matrix(predict(fit.dep1b, newdata = xy), 
                         nrow = grid.lines, ncol = grid.lines)
        # fitted points for droplines to surface
        fitpoints <- predict(fit.dep1b)
        # scatter plot with regression plane
        #options(rgl.useNULL = TRUE)
        scatter3D(
          x = x,
          y = y,
          z = z,
          pch = 21, cex = .4, cex.lab = .7, bty = "b2",
          theta = 25,  phi = -2,
          ticktype = "detailed",
          zlim = c(-4, 21.5),
          xlab = "Anxiety Severity",
          ylab = "Positive Affect",
          zlab = "Depression Severity",
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = NA,
                      col="grey75"),
          #fit = fitpoints),
          colkey = F,
          col = "grey50",
          main = ""
          #plot = T
        )
        plotrgl()
        twonumericint1 <- scene3d()
        rgl.close()
        save <- options(rgl.inShiny = TRUE)
        on.exit(options(save))
        output$twonumeric3 <- renderRglwidget(rglwidget(twonumericint1))

        
      }# finish ifs for twonumeric interaction surface 3D plot 
      
      if (input$ivtypes == 'zerocat' &
          input$twonumericplot == 3 &
          input$twonumericinteraction == 2){
        
        z <- baseline$bods
        x <- baseline$boasev-(mean(baseline$boasev))
        y <- baseline$bpa-(mean(baseline$bpa))
        fit.dep1b <- lm(z~x*y)
        
        anxlow <- min(x)
        anxhigh <- max(x)
        bpaminus <- - sd(baseline$bpa)
        bpaplus <- + sd(baseline$bpa)
        
        # trying a different approach to get the predicted points and lines to add for simple slopes
        # use moderate.lm slopes and intercepts from centered model
        # easier to use the ints and slopes from moderate.lm
        # `pequod` provides the slopes but not intercepts
        ##               INT     Slope         SE       LCL       UC
        ## at zHigh 3.803975 0.3287064 0.09320707 0.1448889 0.5125239
        ## at zMean 5.092850 0.6294323 0.07475149 0.4820118 0.7768528
        ## at zLow  6.381724 0.9301582 0.10703907 0.7190621 1.1412544
        lowanxminusbpa <- 6.381724 + 0.9301582*anxlow
        highanxminusbpa <- 6.381724 + 0.9301582*anxhigh
        lowanxplusbpa <- 3.803975 + 0.3287064*anxlow
        highanxplusbpa <- 3.803975 + 0.3287064*anxhigh
        
        # set up a grid required for the plane drawn by the surface argument of scatter3D
        grid.lines = 21
        x.pred <- seq(min(x), max(x), length.out = grid.lines)
        y.pred <- seq(min(y), max(y), length.out = grid.lines)
        xy <- expand.grid( x = x.pred, y = y.pred)
        
        z.pred <- matrix(predict(fit.dep1b, newdata = xy), 
                         nrow = grid.lines, ncol = grid.lines)
        # fitted points for droplines to surface
        fitpoints <- predict(fit.dep1b)
        # scatter plot with regression plane
        #options(rgl.useNULL = TRUE)
        scatter3D(
          x = x,
          y = y,
          z = z,
          pch = 21, cex = .4, cex.lab = .7, bty = "u",
          theta = 25,  phi = -2,
          ticktype = "detailed",
          zlim = c(-4, 21.5),
          xlab = "Anxiety Severity",
          ylab = "Positive Affect",
          zlab = "Depression Severity",
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = NA,
                      col="grey75"),
          #fit = fitpoints),
          colkey = F,
          col = "grey50",
          main = ""
          #
        )
        
        #Add Arrows(lines) for SME
        
        y0 <- c(bpaminus,bpaplus)
        x0 <- c(anxlow,anxlow)
        z0 <- c(lowanxminusbpa ,lowanxplusbpa )
        y1 <- c(bpaminus,bpaplus)
        x1 <- c(anxhigh,anxhigh)
        z1 <- c(highanxminusbpa,highanxplusbpa  )
        
        
        Colblue <- c("blue","blue")
        arrows3D(x0, y0, z0, x1, y1, z1, 
                 lwd = 5, lty=1, 
                 type="simple", length=.01, col=Colblue,
                 add=T)
        
        # add four points
        xpt <- c(-sd(x), -sd(x), sd(x), sd(x))
        ypt <- c(-sd(y),sd(y),-sd(y),sd(y))
        # taken from pequod output
        zpt <- c(2.9835, 2.6031, 9.7800, 5.0049)
        
        #s_slopes2$Points[1],
        #         s_slopes2$Points[3],
        #         s_slopes2$Points[2],
        #         s_slopes2$Points[4])
        col2 <- c("skyblue","skyblue","dodgerblue","dodgerblue")
        points3D(xpt,ypt,zpt,add=T,cex=2.5,pch=22, 
                 col="black",bg=col2,colkey=F)
        
        plotrgl()
        twonumericint2 <- scene3d()
        rgl.close()
        save <- options(rgl.inShiny = TRUE)
        on.exit(options(save))
        output$twonumeric4 <- renderRglwidget(rglwidget(twonumericint2))
        
      }# finish ifs for twonumeric interaction surface 3D plot plus simple slopes
 
      
    }# finish code inside observeevent
  )#finish observevent

})
