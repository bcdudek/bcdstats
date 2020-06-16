library(shiny)

# Define server logic for slider examples
shinyServer(function(input, output) {

  source("tools.R", local=TRUE)  
  
  # Function to create the graph, based on the graphing function and input
makePlot <- function(){
if (input$control=="slidr"){  
  if (input$plotshownull && input$plotshowalt){
     if (input$tail=="upper"){
       alphabeta2(mu1=input$mean1,mu2=input$mean2,sem=input$sem,type1=input$alpha)
      }
     if (input$tail=="lower"){
	     alphabeta3(mu1=input$mean1b,mu2=input$mean2b,sem=input$semb,type1=input$alpha)
      }
     if (input$tail=="two"){
       alphabeta4(mu1=input$mean1c,mu2=input$mean2c,sem=input$semc,type1=input$alpha)
     }
  }
  if (input$plotshowalt && !isTRUE(input$plotshownull)){
    if (input$tail=="upper"){
      alphabeta2b(mu1=input$mean1,mu2=input$mean2,sem=input$sem,type1=input$alpha)
    }
    if (input$tail=="lower"){
      alphabeta3b(mu1=input$mean1b,mu2=input$mean2b,sem=input$semb,type1=input$alpha)
    }
    if (input$tail=="two"){
      alphabeta4b(mu1=input$mean1c,mu2=input$mean2c,sem=input$semc,type1=input$alpha)
    }
  }
  if (input$plotshownull && !isTRUE(input$plotshowalt)){
    if (input$tail=="upper"){
      alphabeta2c(mu1=input$mean1,mu2=input$mean2,sem=input$sem,type1=input$alpha)
    }
    if (input$tail=="lower"){
      alphabeta3c(mu1=input$mean1b,mu2=input$mean2b,sem=input$semb,type1=input$alpha)
    }
    if (input$tail=="two"){
      alphabeta4c(mu1=input$mean1c,mu2=input$mean2c,sem=input$semc,type1=input$alpha)
    }
  }
}
if (input$control=="menu"){  
  alpha2a <- as.numeric(input$alpha2)
  if (input$plotshownull && input$plotshowalt){
    if (input$tail=="upper"){
      alphabeta2(mu1=input$mean1,mu2=input$mean2,sem=input$sem,type1=alpha2a)
    }
    if (input$tail=="lower"){
      alphabeta3(mu1=input$mean1b,mu2=input$mean2b,sem=input$semb,type1=alpha2a)
    }
    if (input$tail=="two"){
      alphabeta4(mu1=input$mean1c,mu2=input$mean2c,sem=input$semc,type1=alpha2a)
    }
  }
  if (input$plotshowalt && !isTRUE(input$plotshownull)){
    if (input$tail=="upper"){
      alphabeta2b(mu1=input$mean1,mu2=input$mean2,sem=input$sem,type1=alpha2a)
    }
    if (input$tail=="lower"){
      alphabeta3b(mu1=input$mean1b,mu2=input$mean2b,sem=input$semb,type1=alpha2a)
    }
    if (input$tail=="two"){
      alphabeta4b(mu1=input$mean1c,mu2=input$mean2c,sem=input$semc,type1=alpha2a)
    }
  }
  if (input$plotshownull && !isTRUE(input$plotshowalt)){
    if (input$tail=="upper"){
      alphabeta2c(mu1=input$mean1,mu2=input$mean2,sem=input$sem,type1=alpha2a)
    }
    if (input$tail=="lower"){
      alphabeta3c(mu1=input$mean1b,mu2=input$mean2b,sem=input$semb,type1=alpha2a)
    }
    if (input$tail=="two"){
      alphabeta4c(mu1=input$mean1c,mu2=input$mean2c,sem=input$semc,type1=alpha2a)
    }
  }
}

}
# render the graph and make available to the user interface
output$main_plot <- renderPlot({
 makePlot()
  }
 #,height=1000, width=1200
 )
output$downloadPlotpdf <- downloadHandler(
  filename = function() { paste0("test1.pdf") },
  content = function(file1) {
    pdf(file1)
    makePlot()
    dev.off()
  })        
output$downloadPlotpng <- downloadHandler(
  filename = function() { paste0("test1.png") },
  content = function(file1) {
    png(file1)
    makePlot()
    dev.off()
  })   
#Render the text for the about tab  NO LONGER USED
#output$about <- renderRmd('about.Rmd', input)
})


