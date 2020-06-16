library(shiny)
library(Cairo)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  source('tools.R',local=TRUE)
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  makeplot <- function(){
    #print(is.wholenumber(input$df1))
    #print(is.wholenumber(input$df2))
        validate(
      need(is.wholenumber(input$df1), 'Shiny would like you to enter whole numbers for degrees of freedom.
                                      \nConsult a Statistics text or your instructor for information about what fractional degrees of freedom might mean and when to use it.')
      )
    validate(
      need(is.wholenumber(input$df2), 'Shiny would like you to enter whole numbers for degrees of freedom.
           \nConsult a Statistics text or your instructor for information about what fractional degrees of freedom might mean and when to use it.')
      )
    validate(
      need(input$df1 <= 100 & input$df1 >= 1, 'This visualization works best for a relatively small number of numerator degrees of freedom, but it needs to be at least 1.
           \nShiny prefers that you limit numerator df to a maximum of 100')
    )
    validate(
      need(input$df2 <= 1000 & input$df2 >= 2, 'This visualization works best for a limited size of denominator degrees of freedom. It needs to be at least 2.
           \nShiny prefers that you limit denominator df to a maximum of 1000')
    )
    if(input$control=="prob"){
      if(input$entrymode=="slider"){
        qgf(prob=input$pr,df1=input$df1,df2=input$df2)
      }
      else if(input$entrymode=="box"){
        validate(
          need(input$pvalu < .999999999999999 & input$pvalu > .000000000000001, 'Shiny would like you to enter a probability above 0 and less than 1.0 \n(best usage is to no more than 5 decimal places)')
        )
        qgf(pr=input$pvalu,df1=input$df1,df2=input$df2)}}
    
    else if(input$control=="quant"){
      if(input$entrymode=="slider"){  
        pgf(quantile=input$quantile,df1=input$df1,df2=input$df2)}
      else if(input$entrymode=="box"){  
        validate(
          need(input$quantileb <= 100 & input$quantileb > 0, 'For visualization purposes, 
               \nShiny would like you to enter a quantile value greater than zero and no larger than 100')
        )
       pgf(quantile=input$quantileb,df1=input$df1,df2=input$df2)}
    }
  }
#  output$about <- renderRmd('about.Rmd', input)
  output$distPlot <- renderPlot({
    makeplot()
    },width=650)
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
  
})
