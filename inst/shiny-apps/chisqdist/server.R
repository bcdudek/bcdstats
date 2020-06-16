library(shiny)
library(Cairo)




# Define server logic required to generate and plot a random distribution
shinyServer(function(session,input, output) {
  
source('tools.R', local=TRUE)
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  makeplot <- function(){
    validate(
      need(is.wholenumber(input$df), 'Shiny would like you to enter whole numbers for degrees of freedom for this Chi Square app.
           \nConsult a Statistics text or your instructor for information about what fractional degrees of freedom might mean and when to use it.')
      )
    validate(
      need(input$df <= 600, 'This visualization works best for a maximum df  of 600')
    )
    if(input$control=="prob"){
      if(input$entrymode=="slider"){
        qgchisq2(prob=input$pr,df=input$df)
      }
      else if(input$entrymode=="box"){
        validate(
          need(input$pvalu < .999999999999999 & input$pvalu > .000000000000001, 'Shiny would like you to enter a probability above 0 and less than 1.0 \n(best usage is to no more than 5 decimal places)')
        )
        qgchisq2(pr=input$pvalu,df=input$df)}}
    
    else if(input$control=="quant"){
      if(input$entrymode=="slider"){  
        pgchisq2(quantile=input$quantile,df=input$df)}
      else if(input$entrymode=="box"){  
        qmax <- qchisq(.9999995, df=input$df, ncp=0)
        print(qmax)
        validate(
          need(input$quantileb  < qmax, "Your quantile value might be a reasonable number but it is out of shiny's range for this plot.")
        )
        pgchisq2(quantile=input$quantileb,df=input$df)}
    }
  }
  
  #output$about <- renderRmd('about.Rmd', input)
  
  output$distPlot <- renderPlot({
    makeplot()
    }, width=650)
  output$downloadPNG <- downloadHandler(
    filename = function() { paste0("test1.png") },
    content = function(file1) {
      png(file1)
      makeplot()
      dev.off()
    })        
  output$downloadPDF <- downloadHandler(
    filename = function() { paste0("test1.pdf") },
    content = function(file1) {
      CairoPDF(file1)
      makeplot()
      dev.off()
      if (file.exists(paste0(file1, ".pdf")))
        file.rename(paste0(file1, ".pdf"), file1)
    })
  observe({
    startquant <- round(qchisq(.05,df=input$df,lower.tail=F),digits=4)
    qmax <- 5*((2*input$df)^.5)
  updateSliderInput(session, "quantile",
                    value=startquant)
  updateSliderInput(session, "quantileb",
                    value=startquant)
  })
  
})
