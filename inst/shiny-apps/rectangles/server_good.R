source("packages.R")

shinyServer(function(input,output,session){

  source("tools.R",local=T)
  
#--------------------------------------------------------------------------------
# this section has code to define the data for final tabset, bivariate normal
#--------------------------------------------------------------------------------
# output$bivariatedensityPlot <- renderPlot(
#   {
#     bivariateNormal(
#       rho=input$rho,
#       angle=input$angle,
#       layout=c(1,1),
#       colorkey=list(at=seq(0, .28, length=101)),
#       zlim=c(0, .28)
#     )
#   })

#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# This section has code for tabset 3a, the base info/simulation tab
#--------------------------------------------------------------------------------

output$scatter1 <- renderPlot({
  # Plotting Function for simulated data
  corrsim1 <- function(rho1=.7,mx1=20,my1=40,sdx1=2,sdy1=3,n1=50){
    #simulate first data set and set characteristics for graph
    set.seed(input$draw+1)
    a <- rmvnorm(n=n1,mean=c(mx1,my1),sigma=matrix(c(sdx1^2,rho1*(sdx1*sdy1),
                                                     rho1*(sdx1*sdy1),sdy1^2),2,2))
    simdata1 <- as.data.frame(a)
    colnames(simdata1) <- c("x","y")
    #data1
    n1 <- n1
    rho1 <- rho1
    r1 <- round(cor(simdata1$x,simdata1$y),digits=2)
    fit1 <- lm(simdata1$y~simdata1$x)
    yhat1 <- fit1$fitted.values 
    rangey <- max(simdata1$y)- min(simdata1$y)
    top <- min(simdata1$y)+(1.1*rangey)
    bottom <- max(simdata1$y)-(1.15*rangey)
    rangex <- max(simdata1$x)- min(simdata1$x)
    lft <- min(simdata1$x)-(.08*rangex)
    rght <- max(simdata1$x)+(.03*rangex)
    yhatpos <- min(simdata1$x)-(.1*rangex)
    fit1 <- lm(simdata1$y~simdata1$x)   
    op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, 
              mgp = c(3.5, 1, 0), cex.lab = 1.5 , 
              font.lab = 2, cex.axis = 1.3, las=1)
    plot(simdata1$x,simdata1$y,xlab="X",ylab="Y", 
         main=substitute(paste("Simulated Bivariate Distribution Where  ",rho[xy]==rho1)),
         ylim=c(bottom,top),
         xlim=c(lft,rght),
         col="black", pch=21, bg = "grey", cex = 2)
#    abline(h=mean(simdata1$y),lty=2)
#    abline(v=mean(simdata1$x),lty=2)
    if(input$regline1){
      abline(fit1,lwd=2)}
    text((max(simdata1$x))-(.15*rangex), min(simdata1$y)+1.05*rangey, substitute(paste("n=",n1,", ",r[xy]==r1)),
         cex=1.4)
    
  }
  par(las=0) 
  
  ################################################
  
  p <- corrsim1(rho1=input$rho1,mx1=input$mux1,my1=input$muy1,
                sdx1=input$sdx1,sdy1=input$sdy1,n1=input$n1)
})  #finish renderPlot

#output$plot1 <- renderUI({
#  plotOutput("scatter1",width=600,height=600)
#})  #finish output$plot
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
# This section has code for tabset 3b, the quadrants tab
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# first, generate the data

getdata2 <- reactive({
  set.seed(input$draw2+1)
  rho2 <- input$rho2
  n2 <- input$n2
  mx2 <-50
  my2 <- 100
  sdx2 <- 5
  sdy2 <-15
  b <- rmvnorm(n=n2,mean=c(mx2,my2),sigma=matrix(c(sdx2^2,rho2*(sdx2*sdy2),
                                                   rho2*(sdx2*sdy2),sdy2^2),2,2))
  simdata2 <- as.data.frame(b)
  colnames(simdata2) <- c("x","y")
  
  
  simdata2$xdev <- simdata2$x-mean(simdata2$x)
  simdata2$ydev <- simdata2$y-mean(simdata2$y)
  simdata2$cp <- simdata2$xdev*simdata2$ydev
  
  simdata2$quadrant[simdata2$xdev > 0 & simdata2$ydev > 0] <- "Top Right"
  simdata2$quadrant[simdata2$xdev > 0 & simdata2$ydev < 0] <- "Bottom Right"
  simdata2$quadrant[simdata2$xdev < 0 & simdata2$ydev > 0] <- "Top Left"
  simdata2$quadrant[simdata2$xdev < 0 & simdata2$ydev < 0] <- "Bottom Left"
  
  tab1 <- table(simdata2$quadrant)
  tab2 <- as.data.frame(tab1)
  
  tab2$Var1 <- factor(tab2$Var1, levels=
                        c("Top Right","Bottom Left","Top Left", "Bottom Right"),ordered=TRUE)
  tab2 <- tab2[order(tab2$Var1),]
  colnames(tab2) <- c("Quadrant", "Count")
  return(list(simdata2=simdata2,tab2=tab2))
})
#-------------------------------------
corrsim2 <- function(rho2=.7,n2=50){
  datalist <- getdata2()
  simdata2 <- datalist$simdata2
  tab2 <- datalist$tab2  
  n2 <- n2
  rho2 <- rho2
  r2 <- round(cor(simdata2$x,simdata2$y),digits=2)
  fit2 <- lm(simdata2$y~simdata2$x)
  yhat2 <- fit2$fitted.values 
  rangey2 <- max(simdata2$y)- min(simdata2$y)
  top2 <- min(simdata2$y)+(1.1*rangey2)
  bottom2 <- max(simdata2$y)-(1.15*rangey2)
  rangex2 <- max(simdata2$x)- min(simdata2$x)
  lft2 <- min(simdata2$x)-(.08*rangex2)
  rght2 <- max(simdata2$x)+(.03*rangex2)
  yhatpos2 <- min(simdata2$x)-(.1*rangex2)
  #fit2 <- lm(simdata2$y~simdata2$x)   
  op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, 
            mgp = c(3.5, 1, 0), cex.lab = 1.5 , 
            font.lab = 2, cex.axis = 1.3, las=1)
  plot(simdata2$x,simdata2$y,xlab="X",ylab="Y", 
       main=substitute(paste("Simulated Bivariate Distribution Where  ",rho[xy]==rho2)),
       ylim=c(bottom2,top2),
       xlim=c(lft2,rght2),
       col="black", pch=21, bg = "grey", cex = 2)
  abline(h=mean(simdata2$y),lty=2)
  abline(v=mean(simdata2$x),lty=2)
  if(input$regline2){
    abline(fit2,lwd=2)}
  text((max(simdata2$x))-(.15*rangex2), min(simdata2$y)+1.05*rangey2, substitute(paste("n=",n2,", ",r[xy]==r2)),
       cex=1.4)

}

output$scatter2 <- renderPlot({
  # Plotting Function for simulated data
  par(las=0) 
  corrsim2(rho2=input$rho2,n2=input$n2)
})  #finish renderPlot

#output$plot2 <- renderUI({
#  plotOutput("scatter2",width=600,height=600)
#})  #finish output$plot2

output$quadrants1 <- renderTable({
  datalist <- getdata2()
  tab2 <- datalist$tab2  
  tab2},include.rownames=FALSE)

#output$quadrantcounts <- renderUI({
#  tableOutput("quandrants1")
#})

#output$test <- renderUI({
#  tableOutput("quadrants1")
#})


output$tab3boutput <- renderUI({
    if(input$plottype1 == "base"){
       plotOutput("scatter1",width=600,height=600)}
    else if(input$plottype1 == "quadrants"){
      div(
       plotOutput("scatter2",width=600,height=600),
       if(input$quadtable){
       tableOutput("quadrants1")}
       else if(is.na(input$quadtable)){
         return(NULL)
       }
      )
      }
    })  #finish output$tab3abplot 

#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
# this section has code to define the data for tabset on visualizing correlation
#--------------------------------------------------------------------------------
getdata4 <- reactive({
  set.seed(input$draw4+250)
  my4 <- 100
  mx4 <- 50
  sdy4 <- 15
  sdx4 <- 5
  rho <- input$rho4
  n <- input$n4
  a4 <- rmvnorm(n=n,mean=c(my4,mx4),sigma=matrix(c(sdy4^2,rho*(sdy4*sdx4),
                                                   rho*(sdy4*sdx4),sdx4^2),2,2))
  data4b <- as.data.frame(a4)
  colnames(data4b) <- c("y", "x")
  
  xdev <- data4b$x-mean(data4b$x)
  ydev <- data4b$y-mean(data4b$y)
  cp <- xdev*ydev
  data4 <- cbind(data4b,xdev,ydev,cp)
  

  data4$sign <- "positive CP"
  data4$sign[data4$cp < 0] <- "negative CP"
  
  ##################################
  # single value with largest CP
  ##################################
  cpmaxx <- cpmaxy <- NA
  if(rho > 0){
    cpmaxx <- with(data4, x[cp==max(cp)])
    cpmaxy <- with(data4, y[cp==max(cp)])
    cpmaxxdev <- with(data4, xdev[cp==max(cp)])
    cpmaxydev <- with(data4, ydev[cp==max(cp)])
    cpmaxcp <- with(data4, cp[cp==max(cp)])
  }
  else if(rho == 0){
    cpmaxx <- with(data4, x[cp==max(cp)])
    cpmaxy <- with(data4, y[cp==max(cp)])
    cpmaxxdev <- with(data4, xdev[cp==max(cp)])
    cpmaxydev <- with(data4, ydev[cp==max(cp)])
    cpmaxcp <- with(data4, cp[cp==max(cp)])
  }
  else if(rho < 0){
    cpmaxx <- with(data4, x[cp==min(cp)])
    cpmaxy <- with(data4, y[cp==min(cp)])
    cpmaxxdev <- with(data4, xdev[cp==min(cp)])
    cpmaxydev <- with(data4, ydev[cp==min(cp)])
    cpmaxcp <- with(data4, cp[cp==min(cp)])
  }
  
  data4c <- data.frame(Component=c("X Value","X Deviation","Y Value", "Y Deviation", "Cross Product"), 
                       Values=c(cpmaxx,cpmaxxdev,cpmaxy,cpmaxydev,cpmaxcp), 
                       stringsAsFactors = FALSE)
  
  ##################################
  # Pearson and Cov
  ##################################
  pearson <- cor(data4$x,data4$y)
  covxy <-   cov(data4$x,data4$y)
  #pearson
  #covxy
  possp <- sum(data4$cp[data4$sign == "positive CP"])
  negsp <-sum(data4$cp[data4$sign == "negative CP"])
  #possp
  #negsp
  diff <- possp+negsp
  cov2 <- diff/(n-1)
  #cov2

data4b <- data.frame(stats=c("pearson","covxy","positiveSP", "negativeSP","difference", "cov2"), 
                     values=c(pearson,covxy,possp,negsp,diff,cov2), 
                     stringsAsFactors = FALSE)
return(list(data4=data4,data4b=data4b,data4c=data4c,cpmaxx=cpmaxx,cpmaxy=cpmaxy))
})
#-----------------------------------------------------------------------------------------------
# first, a function for ggplot graph in visualizing calc tab
#-----------------------------------------------------------------------------------------------
drawplot4 <- function(rho=.7, n=20){
  require(ggplot2)
  datalist <- getdata4()
  data4 <- datalist$data4
  data4b <- datalist$data4b
  cpmaxx <- datalist$cpmaxx
  cpmaxy <- datalist$cpmaxy
  ##################################
  # GGPLOT base themes
  ##################################
  plotopts1 <- theme_bw() + theme(panel.border = element_blank(), 
                                  panel.grid.major = element_blank(), 
                                  panel.grid.minor = element_blank(), 
                                  axis.line = element_line(colour = "black"),
                                  axis.text=element_text(size=14),
                                  axis.title=element_text(size=16),
                                  title=element_text(size=16)
  )
  plotopts2 <- theme(
    #plot.background = element_rect(fill = "gray85"),
    #panel.border = element_rect(fill = NA, colour = "black",size=.4),
    axis.line = element_line(size = .3, colour = "black", linetype = "solid"),
    panel.background = element_rect(fill = "gray95"),
    axis.text=element_text(size=14),
    axis.title=element_text(size=16),
    title=element_text(size=16)
  )
  ##################################
  
  
  ###########################################
  # Base Scatterplot with deviation arrows
  ###########################################
  labels <- c("X[i] -bar(X)", "Y[i]-bar(Y)")
  rval <- data4b$values[data4b$stats=="pearson"]
  rval2 <- round(rval,digits=4)
  chart_title1 <- substitute(paste("r=",rval2))
  p <- ggplot(data4, aes(x=x, y=y)) + 
    plotopts2 +
    labs(x="X",y="Y") 
  p <- p +  geom_point(shape=1, size=5) +    # Use hollow circles
    geom_point(size=4,colour="orange") 

  p <- p + ggtitle(chart_title1)
  #p <- p +geom_smooth(method=lm, se=FALSE,  fullrange=TRUE,size=1)
  if(input$ybar4){
    p <- p + geom_hline(aes(yintercept = mean(y)), linetype="dashed",size=1)}
  if(input$xbar4){
    p <- p + geom_vline(aes(xintercept = mean(x)), linetype="dashed",size=1)}
  
  if(input$yarrow4){  
    p <- p + geom_segment(x=cpmaxx, y=cpmaxy, xend=cpmaxx, yend=mean(data4$y), 
                          arrow=arrow(), size=1, color="steelblue3",linetype=1) +
      geom_text(x=cpmaxx-(.07*(cpmaxx-mean(data4$x))), y=(mean(data4$y) +((cpmaxy-(mean(data4$y)))/2)), 
                #label="Y - Ybar", 
                label=labels[2],parse=T,
                size=5, colour="black",angle=-90)}
  if(input$xarrow4){    
    p <- p + geom_segment(y=cpmaxy, x=cpmaxx, yend=cpmaxy, xend=mean(data4$x), 
                          arrow=arrow(), size=1, color="steelblue3",linetype=1) +
      geom_text(y=cpmaxy-(.07*(cpmaxy-mean(data4$y))), x=(mean(data4$x) +((cpmaxx-(mean(data4$x)))/2)), 
                #label="X - Xbar", 
                label=labels[1],parse=T,
                size=5, colour="black",angle=0)}
  
  #  p <- p +geom_smooth(method=lm, se=FALSE,  fullrange=TRUE,size=1)
  
  p

} # finish drawplot4 function def

#-----------------------------------------------------------------------------------------------
# function for ggplot graph in visualizing calc tab
#-----------------------------------------------------------------------------------------------

output$viscalc1 <- renderPlot({
  drawplot4(rho=input$rho4,n=input$n4)
})# finish renderPlot for viscalc
output$viscalc <- renderUI({
  plotOutput("viscalc1",width=500,height=500)
})
output$ppm <-renderText({
  datalist <- getdata4()
  data4 <- datalist$data4
  data4b <- datalist$data4b
  data4b$value[data4b$stat=="pearson"]
})
output$note1 <- renderUI({
  if(input$xarrow4 & input$yarrow4){
  HTML("<em>Notice how the arrows form a rectangle with the mean values</em>")
  }
})

output$table4 <- renderTable({
  datalist <- getdata4()
  data4c <- datalist$data4c
  data4c},include.rownames=FALSE)

output$tablevalues4 <- renderUI({
  if(input$cpvalue){
  tableOutput("table4")
  }
   }) #finish renderUI

#--------------------------------------------------------------------------------




#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
# This section has code for tabset 4a, the single  rectangle tab
#--------------------------------------------------------------------------------
# first, data for this simulation.  it is sim #5
#--------------------------------------------------------------------------------
getdata5a <- reactive({
  set.seed(input$draw5+250)
  my5 <- input$muy5
  mx5 <- input$mux5
  sdy5 <- input$sdy5
  sdx5 <- input$sdx5
  rho <- input$rho5
  n5 <- input$n5
  a5 <- rmvnorm(n=n5,mean=c(my5,mx5),sigma=matrix(c(sdy5^2,rho*(sdy5*sdx5),
                                                    rho*(sdy5*sdx5),sdx5^2),2,2))
  data5 <- as.data.frame(a5)
  colnames(data5) <- c("y", "x")
  
  xdev <- data5$x-mean(data5$x)
  ydev <- data5$y-mean(data5$y)
  cp <- xdev*ydev
  data5a <- cbind(data5,ydev,xdev,cp)

  data5a$sign <- "positive CP"
  data5a$sign[data5a$cp < 0] <- "negative CP"
  
  data5a$xminn <- data5a$x
  data5a$yminn <- data5a$y
  data5a$xminn[data5a$xdev > 0] <- mean(data5a$x)
  data5a$yminn[data5a$ydev > 0] <- mean(data5a$y)
  
  data5a$xmaxx <- data5a$x
  data5a$ymaxx <- data5a$y
  data5a$xmaxx[data5a$xdev < 0] <- mean(data5a$x)
  data5a$ymaxx[data5a$ydev < 0] <- mean(data5a$y)
  
  data5a$sign <- "positive CP"
  data5a$sign[data5a$cp < 0] <- "negative CP"
  
  ###########################################################
  # provide capability for separating pos and neg CP
  ###########################################################
  rectp <- as.data.frame(subset(data5a, cp > 0))
  rectp$xminp <- rectp$x
  rectp$yminp <- rectp$y
  rectp$xminp[rectp$xdev > 0] <- mean(data5a$x)
  rectp$yminp[rectp$ydev > 0] <- mean(data5a$y)
  
  rectp$xmaxp <- rectp$x
  rectp$ymaxp <- rectp$y
  rectp$xmaxp[rectp$xdev < 0] <- mean(data5a$x)
  rectp$ymaxp[rectp$ydev < 0] <- mean(data5a$y)
  
  rectn <- as.data.frame(subset(data5a, cp < 0))
  rectn$xminn <- rectn$x
  rectn$yminn <- rectn$y
  rectn$xminn[rectn$xdev > 0] <- mean(data5a$x)
  rectn$yminn[rectn$ydev > 0] <- mean(data5a$y)
  
  rectn$xmaxn <- rectn$x
  rectn$ymaxn <- rectn$y
  rectn$xmaxn[rectn$xdev < 0] <- mean(data5a$x)
  rectn$ymaxn[rectn$ydev < 0] <- mean(data5a$y)
  
  ##################################
  # single point rectangle values
  ##################################
  singleymin <- mean(data5a$y)
  singleymax <- data5a$y[data5a$y == max(data5a$y)]
  
  if(data5a$cp[data5a$y == max(data5a$y)] > 0)
  {singlexmin <- mean(data5a$x)}
  if(data5a$cp[data5a$y == max(data5a$y)] < 0)
  {singlexmin <- data5a$x[data5a$y == max(data5a$y)]}
  
  if(data5a$cp[data5a$y == max(data5a$y)] < 0)
  {singlexmax <- mean(data5a$x)}
  if(data5a$cp[data5a$y == max(data5a$y)] > 0)
  {singlexmax <- data5a$x[data5a$y == max(data5a$y)]}
  
  
  ##################################
  # Pearson and Cov
  ##################################
  pearson <- cor(data5a$x,data5a$y)
  covxy <-   cov(data5a$x,data5a$y)
  #pearson
  #covxy
  possp <- sum(data5a$cp[data5a$sign == "positive CP"])
  negsp <-sum(data5a$cp[data5a$sign == "negative CP"])
  #possp
  #negsp
  diff <- possp+negsp
  cov2 <- diff/(n5-1)
  #cov2

  data5b <- data.frame(stats=c("pearson","covxy","positiveSP", "negativeSP","difference", "cov2"), 
                       values=c(pearson,covxy,possp,negsp,diff,cov2), 
                       stringsAsFactors = FALSE)
  
  #############################################################################
  # pull out values for case with highest Y value for table construction
  #############################################################################
  highyy <- with(data5a, y[y==max(y)])
  highyx <- with(data5a, x[y==max(y)])  
  highydev <- with(data5a, ydev[y==max(y)])
  highyxdev <- with(data5a, xdev[y==max(y)])
  highycp <- with(data5a, cp[y==max(y)])
  data5c <- data.frame(Quantity=c("Y","X","Y deviation","X deviation","Cross Product, area of Rectangle"),
                       Value=c(highyy,highyx,highydev,highyxdev,highycp),
                       stringsAsFactors = FALSE)
  
  return(list(data5a=data5a,data5b=data5b,data5c=data5c,rectp=rectp,rectn=rectn,
              singleymin=singleymin,singleymax=singleymax,
              singlexmin=singlexmin,singlexmax=singlexmax))
})
#-----------------------------------------------------------------------------------------------
# define the drawing function for simulation 5, the single rectangle plot
drawplot5 <- function(rho5=.7,my5= 100,mx5=50,sdy5=15,sdx5=5, n5=20){
  require(ggplot2)
  datalist <- getdata5a()
  data5a <- datalist$data5a
  data5b <- datalist$data5b
  singlexmin <- datalist$singlexmin
  singlexmax <- datalist$singlexmax
  singleymin <- datalist$singleymin
  singleymax <- datalist$singleymax
  
  ##################################
  # GGPLOT base themes
  ##################################
  plotopts1 <- theme_bw() + theme(panel.border = element_blank(), 
                                  panel.grid.major = element_blank(), 
                                  panel.grid.minor = element_blank(), 
                                  axis.line = element_line(colour = "black"),
                                  axis.text=element_text(size=14),
                                  axis.title=element_text(size=16),
                                  title=element_text(size=16)
  )
  plotopts2 <- theme(
    #plot.background = element_rect(fill = "gray85"),
    #panel.border = element_rect(fill = NA, colour = "black",size=.4),
    axis.line = element_line(size = .3, colour = "black", linetype = "solid"),
    panel.background = element_rect(fill = "gray95"),
    axis.text=element_text(size=14),
    axis.title=element_text(size=16),
    title=element_text(size=16)
  )
  ##################################
  
  
  ###########################################
  # Base Scatterplot with single rectangle
  ###########################################
  p <-  ggplot(data5a, aes(x=x, y=y)) + #theme(axis.line=element_line(colour="black")) + 
    plotopts2 +
    labs(x="X",y="Y") +
    geom_point(shape=1, size=5) +    # Use hollow circles
    geom_point(size=4,colour="orange") +
    geom_hline(aes(yintercept = mean(y)), linetype="dashed") + 
    geom_vline(aes(xintercept = mean(x)), linetype="dashed") 
  if(input$showrect1){
    p <- p + geom_rect(xmin=singlexmin, xmax=singlexmax, ymin=singleymin, ymax=singleymax, 
                       color="black", alpha=0.03, fill="cornsilk")}
  if(input$regline5){
    p <- p +geom_smooth(method=lm, se=FALSE,  fullrange=TRUE,size=1)}
  
  p
  
} # finish drawplot5 function def
#-----------------------------------------------------------------------------------------------
# definitions for graph and table output in simulation 5
#-----------------------------------------------------------------------------------------------

output$rectangle5 <- renderPlot({
  drawplot5(rho5=input$rho5,my5=input$muy5, mx5=input$mux5,
            sdy5=input$sdy5,sdx5=input$sdx5,n5=input$n5)
})# finish renderPlot for simulation5
output$rectangle1 <- renderUI({
  plotOutput("rectangle5",width=550,height=550)
})

output$table5 <- renderTable({
  datalist <- getdata5a()
  data5c <- datalist$data5c
  data5c
  },include.rownames=FALSE)

output$tablevalues5 <- renderUI({
  if(input$values5){
    tableOutput("table5")
  }
})

#--------------------------------------------------------------------------------
# This section has code for tabset 4b, the neg/pos  rectangles tab
#--------------------------------------------------------------------------------
# first, data for this simulation.  it is sim #6
#--------------------------------------------------------------------------------
getdata6a <- reactive({
  set.seed(input$draw6+250)
  my6 <- input$muy6
  mx6 <- input$mux6
  sdy6 <- input$sdy6
  sdx6 <- input$sdx6
  rho <- input$rho6
  n6 <- input$n6
  a6 <- rmvnorm(n=n6,mean=c(my6,mx6),sigma=matrix(c(sdy6^2,rho*(sdy6*sdx6),
                                                    rho*(sdy6*sdx6),sdx6^2),2,2))
  

  data6raw <- as.data.frame(a6)
  colnames(data6raw) <- c("y", "x")

  if(input$scaling == "scaled"){
    data6 <- data6raw
  }
  if(input$scaling == "scalefree"){
    zx <- scale(data6raw$x,scale=TRUE)
    zy <- scale(data6raw$y,scale=TRUE)
    data6 <- data.frame(cbind(zy,zx))
    colnames(data6) <- c("y", "x")
  }

  xdev <- data6$x-mean(data6$x)
  ydev <- data6$y-mean(data6$y)
  cp <- xdev*ydev
  data6a <- cbind(data6,ydev,xdev,cp)
  
  
  data6a1 <- data6a
  data6a$cp2 <- abs(data6a$cp)
  data6a$ydev2 <- data6a$ydev + abs(min(data6a$ydev))
  data6a$id1 <- as.factor(seq(1:nrow(data6a)))
  
  data6c <- data6a1
  colnames(data6c) <- c("Y","X","Y dev","X dev","Cross Product")

  
  data6d <- round(data6c,4)
  
  data6a$sign <- "positive CP"
  data6a$sign[data6a$cp < 0] <- "negative CP"
  data6d$sign <- "positive CP"
  data6d$sign[data6a$cp < 0] <- "negative CP"

  data6a$xminn <- data6a$x
  data6a$yminn <- data6a$y
  data6a$xminn[data6a$xdev > 0] <- mean(data6a$x)
  data6a$yminn[data6a$ydev > 0] <- mean(data6a$y)
  
  data6a$xmaxx <- data6a$x
  data6a$ymaxx <- data6a$y
  data6a$xmaxx[data6a$xdev < 0] <- mean(data6a$x)
  data6a$ymaxx[data6a$ydev < 0] <- mean(data6a$y)
  
  data6a$sign <- "positive CP"
  data6a$sign[data6a$cp < 0] <- "negative CP"
  
  ###########################################################
  # provide capability for separating pos and neg CP
  ###########################################################
  rectp <- as.data.frame(subset(data6a, cp > 0))
  rectp$xminp <- rectp$x
  rectp$yminp <- rectp$y
  rectp$xminp[rectp$xdev > 0] <- mean(data6a$x)
  rectp$yminp[rectp$ydev > 0] <- mean(data6a$y)
  
  rectp$xmaxp <- rectp$x
  rectp$ymaxp <- rectp$y
  rectp$xmaxp[rectp$xdev < 0] <- mean(data6a$x)
  rectp$ymaxp[rectp$ydev < 0] <- mean(data6a$y)
  
  rectn <- as.data.frame(subset(data6a, cp < 0))
  rectn$xminn <- rectn$x
  rectn$yminn <- rectn$y
  rectn$xminn[rectn$xdev > 0] <- mean(data6a$x)
  rectn$yminn[rectn$ydev > 0] <- mean(data6a$y)
  
  rectn$xmaxn <- rectn$x
  rectn$ymaxn <- rectn$y
  rectn$xmaxn[rectn$xdev < 0] <- mean(data6a$x)
  rectn$ymaxn[rectn$ydev < 0] <- mean(data6a$y)
  
  
  
  ##################################
  # Pearson and Cov
  ##################################
  pearson <- cor(data6a$x,data6a$y)
  covxy <-   cov(data6a$x,data6a$y)
  possp <- sum(data6a$cp[data6a$sign == "positive CP"])
  negsp <-sum(data6a$cp[data6a$sign == "negative CP"])
  diff <- possp+negsp
  cov2 <- diff/(n6-1)

  
  data6b <- data.frame(Statistics=c("Pearson Correlation","Covariance of X and Y","Total SP",
                                    "Positive SP", "Negative SP"), 
                       Values=c(pearson,covxy,diff,possp,negsp), 
                       stringsAsFactors = FALSE)
  
  return(list(data6a=data6a,data6b=data6b,data6c=data6c,data6d=data6d,rectp=rectp,rectn=rectn))
})
#--------------------------------------------------------------------------------

# define the drawing function for simulation 6, the pos/neg
drawplot6 <- function(rho6=.7,my6= 100,mx6=50,sdy6=15,sdx6=5, n6=20){
  require(ggplot2)
  datalist <- getdata6a()
  data6a <- datalist$data6a
  data6b <- datalist$data6b
  rectp <- datalist$rectp
  rectn <- datalist$rectn
  

  ##################################
  # GGPLOT base themes
  ##################################
  plotopts1 <- theme_bw() + theme(panel.border = element_blank(), 
                                  panel.grid.major = element_blank(), 
                                  panel.grid.minor = element_blank(), 
                                  axis.line = element_line(colour = "black"),
                                  axis.text=element_text(size=14),
                                  axis.title=element_text(size=16),
                                  title=element_text(size=16)
  )
  plotopts2 <- theme(
    #plot.background = element_rect(fill = "gray85"),
    #panel.border = element_rect(fill = NA, colour = "black",size=.2),
    axis.line = element_line(size = .3, colour = "black", linetype = "solid"),
    panel.background = element_rect(fill = "gray95"),
    axis.text=element_text(size=14),
    axis.title=element_text(size=16),
    title=element_text(size=16)
  )
  ##################################
  
  
  ###############################################
  # positive, then negative, then both rectangles
  ###############################################
  if(input$plottype6=="base"){ 
  p <- ggplot(data6a, aes(x=x, y=y)) + #theme(axis.line=element_line(colour="black")) + 
    plotopts2 +
    labs(x="X",y="Y") +
    geom_point(shape=1, size=5) +    # Use hollow circles
    geom_point(size=4,colour="orange") +
    geom_hline(aes(yintercept = mean(y)), linetype="dashed") + 
    geom_vline(aes(xintercept = mean(x)), linetype="dashed")
  if(input$regline6){
    p <- p +  geom_smooth(method=lm, se=FALSE,fullrange=TRUE)}
  p}
  else if(input$plottype6=="positive6"){
  p <- ggplot(data6a, aes(x=x, y=y)) + #theme(axis.line=element_line(colour="black")) + 
      plotopts2 +
      labs(x="X",y="Y") +
      geom_point(shape=1, size=5) +    # Use hollow circles
      geom_point(size=4,colour="orange") +
      geom_hline(aes(yintercept = mean(y)), linetype="dashed") + 
      geom_vline(aes(xintercept = mean(x)), linetype="dashed") +
      geom_rect(data=rectp, 
                mapping=aes(xmin=xminp, xmax=xmaxp, ymin=yminp, ymax=ymaxp), 
                color="black", alpha=0.15, fill="skyblue")
    if(input$regline6){
      p <- p +  geom_smooth(method=lm, se=FALSE,fullrange=TRUE)}
  p
  }
  else if(input$plottype6 == "negative6"){
    p <-   p <- ggplot(data6a, aes(x=x, y=y)) + #theme(axis.line=element_line(colour="black")) + 
      plotopts2 +
      labs(x="X",y="Y") +
      geom_point(shape=1, size=5) +    # Use hollow circles
      geom_point(size=4,colour="orange") +
      geom_hline(aes(yintercept = mean(y)), linetype="dashed") + 
      geom_vline(aes(xintercept = mean(x)), linetype="dashed") +
      geom_rect(data=rectn, 
                       mapping=aes(xmin=xminn, xmax=xmaxn, ymin=yminn, ymax=ymaxn), 
                       color="black", alpha=0.15,fill="red")
    if(input$regline6){
      p <- p +  geom_smooth(method=lm, se=FALSE,fullrange=TRUE)}
    p}
  else if(input$plottype6=="both6"){
    p <-   p <- ggplot(data6a, aes(x=x, y=y)) + #theme(axis.line=element_line(colour="black")) + 
      plotopts2 +
      labs(x="X",y="Y") +
      geom_point(shape=1, size=5) +    # Use hollow circles
      geom_point(size=4,colour="orange") +
      geom_hline(aes(yintercept = mean(y)), linetype="dashed") + 
      geom_vline(aes(xintercept = mean(x)), linetype="dashed") +
      geom_rect(data=rectn, 
                       mapping=aes(xmin=xminn, xmax=xmaxn, ymin=yminn, ymax=ymaxn), 
                       color="black", fill="red", alpha=0.15) +
             geom_rect(data=rectp, 
                       mapping=aes(xmin=xminp, xmax=xmaxp, ymin=yminp, ymax=ymaxp), 
                      color="black", fill="skyblue", alpha=0.12) + theme(legend.position="none")
    if(input$regline6){
      p <- p +  geom_smooth(method=lm, se=FALSE,fullrange=TRUE)}
  p}
  else if(input$plottype6=="hist"){ 
## NEED THREE HISTORGRAMS, ONE FOR RHO=1, ONE FOR RHO=-1 AND ONE FOR THE REST
    p <- ggplot(data=data6a, aes(cp, fill=sign)) + 
      plotopts2 + 
      labs(x="Cross Product Value", y="Count", title="Frequency Histogram of Cross Product Values") +
      geom_histogram(color="black", alpha=.7) + theme(legend.position="none")
    p
    p}
  else if(input$plottype6 == "packed"){
    ## NEED THREE TREEMAPS, ONE FOR RHO=1, ONE FOR RHO=-1 AND ONE FOR THE REST
    packedrectangle <- treemap(data6a, index="id1",vSize="cp2", vColor="sign",type="categorical",title="Cross Product Rectangles")
    lapply(tail(grid.ls(print=FALSE)$name, 1), grid.remove)
  }
} # finish drawplot6 function def

#--------------------------------------------------------------------------------
# generate output plots and tables for simulation 6
#--------------------------------------------------------------------------------

output$rectangle6 <- renderPlot({
  drawplot6(rho6=input$rho6,my6=input$muy6, mx6=input$mux6,
            sdy6=input$sdy6,sdx6=input$sdx6,n6=input$n6)
})# finish renderPlot for simulation6
output$rectangles <- renderUI({
  plotOutput("rectangle6",width=600,height=600)
})

output$table6a <- renderDataTable({
  datalist <- getdata6a()
  data6d <- datalist$data6d}, options=list(pageLength=25)
)

output$tablevalues6 <- renderUI({
  if(input$showdata6=="data"){
    dataTableOutput("table6a")}
  else if(input$showdata6=="summary"){
    return(NULL)}
  else if(input$showdata6=="none"){
    return(NULL)}
})

output$table6b <- renderTable({
  datalist <- getdata6a()
  data6b <- datalist$data6b}
)

output$tablevalues6b <- renderUI({
  if(input$showdata6=="summary"){
    tableOutput("table6b")}
  else if(input$showdata6=="data"){
    return(NULL)}
  else if(input$showdata6=="none"){
    return(NULL)}
})

#--------------------------------------------------------------------------------
# generate markdown output for the about tab Nolonger using Rmd
#--------------------------------------------------------------------------------

#output$about <- renderRmd('about.Rmd', input)


})#complete shiny server
