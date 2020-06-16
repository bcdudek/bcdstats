#require(shiny)
library(Cairo)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
source('tools.R', local=TRUE)  

    # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  makeplot <- function(inputs,outputs){
    validate(
      need(input$df <= 750 & input$df >=1, 'Shiny cannot draw a graph unless df is one or greater.
           \nFor the most useful visualizations, Shiny would like you to enter df less than 750.
            \nTextbooks often say that the t is well approximated by a std normal.  
             \nHowever, even for df=300 the discrepancy may be as much as .4%.  
             \nTherefore this app permits df up to 750 where the approximation by std normal is very good')
    )
    if(input$control=="prob"){
      if(input$entrymode=="slider"){
        if(input$tail=="upper"){
          qgt(pr=input$pru,tail=input$tail,df=input$df)}
        else if(input$tail=="lower"){
          qgt(pr=input$prl,tail=input$tail,df=input$df)}
        else if(input$tail=="two"){
          qgt(pr=input$prt,tail=input$tail,df=input$df)}
      }
      else if(input$entrymode=="box"){
        if(input$tail=="upper"){
          validate(
            need(input$pvalu < .999999999999999 & input$pvalu > .000000000000001, 'Shiny would like you to enter a probability above 0 and less than 1.0 \n(best usage is to no more than 5 decimal places)')
          )
          qgt(pr=input$pvalu,tail=input$tail,df=input$df)}
        else if(input$tail=="lower"){
          validate(
            need(input$pvall < .999999999999999 & input$pvall > .000000000000001, 'Shiny would like you to enter a probability above 0 and less than 1.0 \n(best usage is to no more than 5 decimal places)')
          )
          qgt(pr=input$pvall,tail=input$tail,df=input$df)}
        else if(input$tail=="two"){
          validate(
            need(input$pvalt < .999999999999999 & input$pvalt > .000000000000001, 'Shiny would like you to enter a probability above 0 and less than 1.0 \n(best usage is to no more than 5 decimal places)')
          )
          qgt(pr=input$pvalt,tail=input$tail,df=input$df)}    
      }
    }
    
    else if(input$control=="quant"){
      if(input$entrymode=="slider"){  
        if(input$tail=="upper"){
          pgt(quantile=input$quantileu,tail=input$tail,df=input$df)}
        else if(input$tail=="lower"){
          pgt(quantile=input$quantilel,tail=input$tail,df=input$df)}
        else if(input$tail=="two"){
          pgt(quantile=input$quantilet,tail=input$tail,df=input$df)}
      }
      if(input$entrymode=="box"){  
        if(input$tail=="upper"){
          validate(
            need(input$quantileub  >= -5.3 & input$quantileub  <= 5.3, 'For a useful visualization, Shiny prefers quantile values between -5.3 and +5.3')
          )          
          pgt(quantile=input$quantileub,tail="upper",df=input$df)}
        else if(input$tail=="lower"){
          validate(
            need(input$quantilelb  >= -5.3 & input$quantilelb  <= 5.3, 'For a useful visualization, Shiny prefers quantile values between -5.3 and +5.3')
          )
          pgt(quantile=input$quantilelb,tail="lower",df=input$df)}
        else if(input$tail=="two"){
          validate(
            need(input$quantiletb  >= 0 & input$quantiletb  <= 5.3, 
'For a useful visualization in this two-tailed situation,
Shiny prefers quantile values between 0 and +5.3.  
If you have a negative quantile and want the two-tailed 
probability, then enter the positive value of that 
number and both positive and negative areas 
will be demarcated/displayed.')
          )
          pgt(quantile=input$quantiletb,tail="two",df=input$df)}
      }
      
    }
  }
#  output$about <- renderRmd('about.Rmd', input)
  
  output$distPlot <- renderPlot({
    makeplot()
    },width=600)
  
  output$downloadPNG <- downloadHandler(
    filename = function() { 
#      paste0("test1.png") 
      paste0(format(Sys.time(), "%Y-%m-%d:%T-%p"),round(runif(1,1,200),0), ".png")      
      },
    content = function(file1) {
      png(file1)
      makeplot()
      dev.off()
    })        
  output$downloadPDF <- downloadHandler(
    filename = function() { 
#      paste0("test1.pdf") 
      paste0(format(Sys.time(), "%Y-%m-%d:%T-%p"),round(runif(1,1,200),0), ".pdf")      
      },
    content = function(file1) {
      CairoPDF(file1)
      makeplot()
      dev.off()
#      if (file.exists(paste0(file1, ".pdf")))
#        file.rename(paste0(file1, ".pdf"), file1)
    })        
  
})