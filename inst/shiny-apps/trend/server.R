source("packages.R")

# Define server logic required create and view the plot
shinyServer(function(input, output) {
#int <- 10000
#linwt <- 1000
#input$quadwt <- 1000
#cubicwt <- 0
#input$quartwt <- 0



output$plot <- renderPlot({
  int <- 175
  pred0 <- int + input$linwt*-2 + input$quadwt*2 + input$cubicwt*-1 + input$quartwt*1 
  pred1 <- int + input$linwt*-1 + input$quadwt*-1 + input$cubicwt*2 + input$quartwt*-4 
  pred2 <- int + input$linwt*0 + input$quadwt*-2 + input$cubicwt*0 + input$quartwt*6 
  pred3 <- int + input$linwt*1 + input$quadwt*-1 + input$cubicwt*-2 + input$quartwt*-4 
  pred4 <- int + input$linwt*2 + input$quadwt*2 + input$cubicwt*1 + input$quartwt*1 
  group <- c(0,1,2,3,4)
  means <- c(pred0,pred1,pred2,pred3,pred4)
  plot(group,means,type="l", 
       xlab="IV Level, eg., dose",
       ylab="DV Value",
       ylim=c(0,380),
       cex.axis=1.4,cex.lab=1.4)
  points(group,means,cex=2)
},height=400,width=525)

output$tab1 <- renderTable({
  int <- 175
  pred0 <- int + input$linwt*-2 + input$quadwt*2 + input$cubicwt*-1 + input$quartwt*1 
  pred1 <- int + input$linwt*-1 + input$quadwt*-1 + input$cubicwt*2 + input$quartwt*-4 
  pred2 <- int + input$linwt*0 + input$quadwt*-2 + input$cubicwt*0 + input$quartwt*6 
  pred3 <- int + input$linwt*1 + input$quadwt*-1 + input$cubicwt*-2 + input$quartwt*-4 
  pred4 <- int + input$linwt*2 + input$quadwt*2 + input$cubicwt*1 + input$quartwt*1 
  group <- c(0,1,2,3,4)
  means <- c(pred0,pred1,pred2,pred3,pred4)
  dev1 <- (pred0-int)**2
  dev2 <- (pred1-int)**2
  dev3 <- (pred2-int)**2
  dev4 <- (pred3-int)**2
  dev5 <- (pred4-int)**2
  bgss <- sum(dev1+dev2+dev3+dev4)
  lin <- c(-2,-1,0,1,2)
  quad <- c(2,-1,-2,-1,2)
  cubic <- c(-1,2,0,-2,1)
  quartic <- c(1,-4,6,-4,1)
  rlin <- cor(lin,means)
  linss <- (rlin**2)*bgss 
  rquad <- cor(quad,means)
  quadss <- (rquad**2)*bgss 
  rcubic <- cor(cubic,means)
  cubicss <- (rcubic**2)*bgss 
  rquartic <- cor(quartic,means)
  quarticss <- (rquartic**2)*bgss 
  linpct <- round(linss/bgss,3)*100
  quadpct <- round(quadss/bgss,3)*100
  cubicpct <- round(cubicss/bgss,3)*100
  quarticpct <- round(quarticss/bgss,3)*100
  Component <- c("Linear","Quadratic","Cubic","Quartic")
  pct <- c(linpct,quadpct,cubicpct,quarticpct)
  tab1.df <- data.frame(cbind(Component,pct))
  colnames(tab1.df) <- c("Component", "Percentage of BG SS")
  tab3.df <- tab1.df
},digits=c(0,0,2),include.rownames=FALSE)

output$tab2 <- renderTable({
  Group <- c(1,2,3,4,5)
  Linear <- c(-2,-1,0,1,2)
  Quadratic <- c(2,-1,-2,-1,2)
  Cubic <- c(-1,2,0,-2,1)
  Quartic <- c(1,-4,6,-4,1)
  tab2.df <- data.frame(cbind(Group,Linear,Quadratic,Cubic,Quartic))
},digits=c(0,0,0,0,0,0),include.rownames=FALSE)



})

