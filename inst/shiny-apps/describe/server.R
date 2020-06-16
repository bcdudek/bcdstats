# shiny server side code 

# first set environment, loading packages and functions
source("packages.R")

shinyServer(function(input, output, session){
  source("tools.R", local=TRUE)
  
    #update variable and group based on dataset
  #output$about <- renderRmd('about.Rmd', input)

observe({
  if (is.null(input$dataset))
    return()
    obj<-switch(input$dataset,
                "aron1_5" = aron1_5,
                "mouse1" = mouse1,
                "morning1" = morning1,
                "faithful2" = faithful2)  
   var.opts <- namel(colnames(obj))
   updateSelectInput(session, "variable",
                     label=paste("Choose Variable:"),
                     choices = var.opts)
})  

  output$caption<-renderText({
    switch(input$plottype,
           "boxplot" = "Boxplot",
           "histogram" =	"Frequency Histogram",
           "eahist" = "Equal Area Histogram",
           "dchist" = "Diagonally Cut Histogram, Denby & Mallows, 2009",
           "qqp" = "QQ Normal Plot with a 95% CI envelope",
           "violin" = "Violin Plot")
  })
# gwidth helps set the graph width in the output window
  gwidth <- function(){
    if (input$plottype == "boxplot") return(300)
    else if (input$plottype == "histogram") return(600)
    else if (input$plottype == "eahist") return(600)
    else if (input$plottype == "dchist") return(600)
    else if (input$plottype == "qqp") return(600)
    else if (input$plottype == "violin") return(600)
  }
  
  #plotting function using base graphics system
  output$p <- renderPlot({
    variable <- get(input$dataset)[[input$variable]]
    if (is.null(variable))
      return(NULL)    
    
    plot.obj<<-list() # not sure why input$X can not be used directly?
    plot.obj$data<<-get(input$dataset)
    plot.obj$variable<<-with(plot.obj$data,get(input$variable))
    
    #dynamic plotting options
      if(input$plottype=="boxplot")  {  #control for plot type
        p<- boxplot(plot.obj$variable,main="",ylab=input$variable)
        rug(jitter(plot.obj$variable), side=4,col="red")
      }
      
      else if(input$plottype=="histogram")  { 
       if(input$breaks=="custom")
         {brk <- input$breakCount}
       else if(input$breaks != "custom")
       {brk <- input$breaks}
       p<- hist(plot.obj$variable,main="",xlab=input$variable,
                 breaks=brk,prob=FALSE)
        rug(jitter(plot.obj$variable),col="red")
        if(input$density){
          p<- hist(plot.obj$variable,main="",xlab=input$variable,
                   breaks=brk,prob=TRUE)
          rug(jitter(plot.obj$variable),col="red")
             dens1 <- density(plot.obj$variable)
             dens1$defaultbw <- dens1$bw
             dens <- density(plot.obj$variable,
                            bw = input$bw_adjust)
            lines(dens, col = "blue") 
        }
      }

    else if(input$plottype=="eahist")  { 
      p <- dhist(plot.obj$variable,xlab=input$variable,a=1000)
      }

    else if(input$plottype=="dchist")  { 
      p <- dhist(plot.obj$variable,xlab=input$variable)
    }
    
    else if(input$plottype=="qqp")  { 
      p <- qqPlot(plot.obj$variable,ylab=input$variable)
    }
    
    else if(input$plottype=="violin")  { 
      p <-vioplot(plot.obj$variable,col="lightblue",horizontal=TRUE,names="")
    }
    
    
   },width=gwidth)

  
output$plot <- renderUI({
    plotOutput("p")
  })

  #  output$d <- renderPrint({
#    print.obj<<-list()
#    print.obj$data<<-get(input$dataset)
#    print.obj$variable<<-with(print.obj$data,get(input$variable))
#     desc <- describe(print.obj$variable,type=2)
#    desc2 <- as.data.frame(c(desc[2:6],desc[8:13]))
#     colnames(desc2)<- c("n","mean","std dev","median","trimmed mean","minimum","maximum","range","skewness","kurtosis","SEM")
#     rownames(desc2) <- c(" ")
#    #print(desc2)
#    desc2
#  })
  
  output$tab1 <- renderTable({
    variable <- get(input$dataset)[[input$variable]]
    if (is.null(variable))
      return(NULL)    
    
    print.obj<<-list()
    print.obj$data<<-get(input$dataset)
    print.obj$variable<<-with(print.obj$data,get(input$variable))
    desc <- describe(print.obj$variable,type=2)
    desc2 <- as.data.frame(c(desc[2:6],desc[8:13]))
    colnames(desc2)<- c("n","mean","std dev","median","10% trimmed mean","minimum","maximum","range","skewness","kurtosis","SEM")
    rownames(desc2) <- c(" ")
    desc2
    #print(tab1.df)
     },include.rownames=FALSE)
  

#data1 <- read.csv(file.choose())
#dd <- density(data1$Number.of.Social.Interactions)  
#ddx <- dd$bw
#ddx

  output$sliderSetup <- renderUI({
    variable <- get(input$dataset)[[input$variable]]
    if (is.null(variable))
      return(NULL)    
    
    bw.obj<<-list() # not sure why input$X can not be used directly?
    bw.obj$data<<-get(input$dataset)
    bw.obj$variable<<-with(bw.obj$data,get(input$variable))
    dd <- density(bw.obj$variable)
    ddx <- dd$bw
    return(sliderInput("bw_adjust", 
                       "Kernel Density Bandwidth Adjustment:", 
                       min=.02, max=10, value=ddx, step=.01))
  })
   output$ddx2 <- renderText({
     variable <- get(input$dataset)[[input$variable]]
     if (is.null(variable))
       return(NULL)    
    plot.obj<<-list() # not sure why input$X can not be used directly?
    plot.obj$data<<-get(input$dataset)
    plot.obj$variable<<-with(plot.obj$data,get(input$variable))
    dd <- density(plot.obj$variable)
    ddx <- dd$bw
    ddxx <- paste("Recommended Bandwidth (Silverman) = ",round(ddx,6))
    ddxx
    })
  
  #output$about <- renderRmd('about.Rmd', input)  
 # output$aboutthedata <- renderRmd('aboutdata.Rmd', input)
  output$kde <- renderUI({
    a(paste0("What is a Kernel Density Function?"), 
      href=paste0("http://en.wikipedia.org/wiki/Kernel_density_estimation"),target="_blank")
     })
  output$gtype <-renderUI({
        if (input$plottype == "boxplot")
          gtype= "a Boxplot"
        if (input$plottype == "histogram")
          gtype= "a Frequency Histogram"
        if (input$plottype == "eahist") 
          gtype= "an Equal Area Histogram"
        if (input$plottype == "dchist") 
          gtype= "a Diagonally Cut Histogram"
        if (input$plottype == "qqp") 
          gtype= "a Normal QQ Plot"
        if (input$plottype == "violin")
          gtype= "a Violin Plot"

        if (input$plottype == "boxplot")
          url1="http://en.wikipedia.org/wiki/Boxplot"
        if (input$plottype == "histogram")
          url1="http://en.wikipedia.org/wiki/Histogram"
        if (input$plottype == "eahist") 
          url1="http://www.albany.edu/psychology/bcd/eahist.pdf"
        if (input$plottype == "dchist") 
          url1="http://www.tandfonline.com/doi/abs/10.1198/jcgs.2009.0002#.UmreuBAucig"
        if (input$plottype == "qqp") 
          url1="http://en.wikipedia.org/wiki/Normal_probability_plot"
        if (input$plottype == "violin")
          url1="http://en.wikipedia.org/wiki/Violin_plot"
        
        
       a(paste0("What is ", gtype,"?"), href=paste0(url1),target="_blank")
      })
  
  
#  output$grtype <- renderText({
#   if (input$plottype == "boxplot") gtype="Boxplot"
#    else if (input$plottype == "histogram") gtype="Frequency Histogram?"
#    else if (input$plottype == "eahist") gtype="Equal Area Histogram?"
#    else if (input$plottype == "dchist") gtype="Diagonally Cut Histogram?"
#    else if (input$plottype == "qqp") gtype="QQ Plot?"
#    return(gtype)
#    
#  })
  })