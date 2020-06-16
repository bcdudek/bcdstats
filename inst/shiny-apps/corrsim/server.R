#####
# DEC 5 note....
# working, but havn't implemented using var names from data frame
# when no header is included in csv

# first set environment, loading packages and functions
        
source("packages.R")

shinyServer(function(input, output, session){
source("tools.R", local=T)
  
#output$about <- renderRmd('about.Rmd', input)
#output$uploaddata <- renderRmd('uploaddata.Rmd', input)
getFile <- reactive({
    if(!is.null(input$upload)){
      read.csv(input$upload$datapath,header=input$header)
    }
  })

output$varsets <- renderUI({
  data1 <- getFile()  ## to be explained below
  if(is.null(data1)){
    return(NULL)
  }
  else if(!is.null(data1)){
    list(
      helpText("Choose two numeric variables for the scatterplot:"),
      selectInput("column_x", "Column x:", choices=names(data1)),      
      selectInput("column_y", "Column y:", choices=names(data1)),
      actionButton("gobutton", "Plot It!")
    )
  }
})
    
  output$p <- renderPlot({
    #control for plot type
      if(input$plottype=="sim")  { 
        ################################################
        # Plotting Function for simulated data
        corrsim <- function(rho1=.7,m1=20,m2=40,sd1=2,sd2=3,n=50){
          #simulate first data set and set characteristics for graph
          set.seed(input$draw+1)
          a <- rmvnorm(n=n,mean=c(m1,m2),sigma=matrix(c(sd1^2,rho1*(sd1*sd2),
                                                        rho1*(sd1*sd2),sd2^2),2,2))
          data2 <- as.data.frame(a)
          colnames(data2) <- c("x","y")
          #data1
          n <- n
          rho1 <- rho1
          r1 <- round(cor(data2$x,data2$y),digits=2)
          fit1 <- lm(data2$y~data2$x)
          yhat1 <- fit1$fitted.values 
          rangey <- max(data2$y)- min(data2$y)
          top <- min(data2$y)+(1.1*rangey)
          bottom <- max(data2$y)-(1.15*rangey)
          rangex <- max(data2$x)- min(data2$x)
          lft <- min(data2$x)-(.08*rangex)
          rght <- max(data2$x)+(.03*rangex)
          yhatpos <- min(data2$x)-(.1*rangex)
          fit1 <- lm(data2$y~data2$x)   
          sdlineap <- mean(data2$y)-((sd(data2$y)/sd(data2$x))*mean(data2$x))
          sdlinean <- mean(data2$y)+((sd(data2$y)/sd(data2$x))*mean(data2$x))
          #if(cov(data2$x,data2$y)<0){
          #  sdlinea=sdlinean}
          #if(cov(data2$x,data2$y)>0){
          #  sdlinea=sdlineap}
          #if(cov(data2$x,data2$y)==0){
          #  sdlinea=mean(data2$y)}
          #if(cov(data2$x,data2$y)==0){
          #  sdlineb=0}          
          sdlinebp <- sd(data2$y)/sd(data2$x)
          sdlinebn <- -sd(data2$y)/sd(data2$x)          
          #if(cov(data2$x,data2$y)<0){
          #  sdlineb=sdlinebn}
          #if(cov(data2$x,data2$y)>0){
          #  sdlineb=sdlinebp}
          op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, 
                    mgp = c(3.5, 1, 0), cex.lab = 1.5 , 
                    font.lab = 2, cex.axis = 1.3, las=1)
          plot(data2$x,data2$y,xlab="X",ylab="Y", 
               main=substitute(paste("Simulated Bivariate Distribution Where  ",rho[xy]==rho1)),
               ylim=c(bottom,top),
               xlim=c(lft,rght),
               col="black", pch=21, bg = "grey", cex = 2)
          text((min(data2$x) + (.9*rangex)), 
                (min(data2$y)+(1.05*rangey)), 
               substitute(paste("n=",n,", ",r[xy]==r1)), cex=1.4)
          abline(h=mean(data2$y),lty=2)
          abline(v=mean(data2$x),lty=2)
          if(input$regline){
            abline(fit1,lwd=2)}
          if(input$sdlinepos){
            abline(a=sdlineap,b=sdlinebp, col="blue",lwd=3)}
          if(input$sdlineneg){
            abline(a=sdlinean,b=sdlinebn, col="blue",lwd=3)}
          if(input$rugs){
          rug(yhat1,side=2,col="darkred",ticksize=.02,lty=1,lwd=1.75,pos=yhatpos)
          rug(data2$y,side=2,col="black",ticksize=.02,lwd=1.75)
          legend("topleft", legend=paste(" Black Rug Plot: Y values\n", 
                                         "Red Rug Plot:   Yhat values"),inset=.0005,bty="n",cex=1)
          text(min(data2$x),bottom,  adj=c(0,0), col="darkred", 
               label=substitute(paste("note that "(s[Yhat]^2/s[Y]^2),"=",r[XY]^2)),cex=1.2)
          
          }
          par(las=0) 
        }
        ################################################
        
        p<- corrsim(rho1=input$rho1,m1=input$mux,m2=input$muy,
                   sd1=input$sdx,sd2=input$sdy,n=input$n)
         }
        
      else if(input$plottype=="data"){ 
          if (is.null(input$upload)) {
            return(NULL)
          }
          else if(!is.null(input$upload)){
            if(!is.null(input$gobutton)){
              if(input$gobutton==0) return(NULL)
              plot.obj<-list() # not sure why input$X can not be used directly?
              plot.obj$data<-getFile()
              plot.obj$variablex<-with(plot.obj$data,get(input$column_x))
              plot.obj$variabley<-with(plot.obj$data,get(input$column_y))
              namex <- input$column_x
              namey <- input$column_y
          op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, 
                mgp = c(3.5, 1, 0), cex.lab = 1.5 , 
                font.lab = 2, cex.axis = 1.3, las=1)
          ################################################
          # PLOTTING FUNCTION for uploaded data
          plotcorr <- function(x,y){
            #colnames(data1) <- c("x","y")
            r1 <- round(cor(plot.obj$variablex,plot.obj$variabley),digits=2)
            fit1 <- lm(plot.obj$variabley~plot.obj$variablex)
            yhat1 <- fit1$fitted.values 
            rangey <- max(plot.obj$variabley)- min(plot.obj$variabley)
            top <- min(plot.obj$variabley)+(1.1*rangey)
            bottom <- max(plot.obj$variabley)-(1.15*rangey)
            rangex <- max(plot.obj$variablex)- min(plot.obj$variablex)
            lft <- min(plot.obj$variablex)-(.08*rangex)
            rght <- max(plot.obj$variablex)+(.03*rangex)
            yhatpos <- min(plot.obj$variablex)-(.1*rangex)
            plot(plot.obj$variablex,plot.obj$variabley, 
                 main=substitute(paste("Bivariate Scatterplot")),
                 ylim=c(bottom,top),
                 xlim=c(lft,rght),
                 ylab=namey,
                 xlab=namex,
                 col="black", pch=21, bg = "grey", cex = 2)
            text((min(plot.obj$variablex)+.93*rangex), min(plot.obj$variabley)+1.05*rangey, 
                 substitute(paste(r[xy]==r1)),cex=1.4)
            abline(h=mean(plot.obj$variabley),lty=2)
            abline(v=mean(plot.obj$variablex),lty=2)
            if(input$regline2){
              abline(fit1,lwd=1.75)}
            if(input$loess2){
              fitloess<-loess(plot.obj$variabley~plot.obj$variablex)
              hat <- predict(fitloess)
              lines(plot.obj$variablex[order(plot.obj$variablex)],
                    hat[order(plot.obj$variablex)],
                    col="blue",lwd=2)} 
            if(input$rugs2){
              rug(yhat1,side=2,col="darkred",ticksize=.02,lty=1,lwd=1.7,pos=yhatpos)
              rug(plot.obj$variabley,side=2,col="black",ticksize=.02,lwd=1.7)
              legend("topleft", legend=paste(" Black Rug Plot: Y values\n", 
                                             "Red Rug Plot:   Yhat values"),inset=.005,bty="n",cex=1.05)
              text(min(plot.obj$variablex),bottom,  adj=c(0,0), col="darkred", 
                   label=substitute(paste("note that "(s[Yhat]^2/s[Y]^2),"=",r[XY]^2)),cex=1.2)
            }
            par(las=0)
          }
          ################################################          
          
          p <- plotcorr(plot.obj$variablex,plot.obj$variabley)
          }
          }

        }
      })

  
  output$plot <- renderUI({
    plotOutput("p",width=600,height=600)
  })

  })
