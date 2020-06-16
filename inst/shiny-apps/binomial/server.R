
# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  source('tools.R',local=TRUE)
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 

  makePlot <- function(){
    #print(input$size)
    #print(class(input$size))
    validate(
      need(is.wholenumber(input$size), 'Shiny would like you to enter whole numbers for the number of trials.')
    )
    validate(
      need(input$size >=1 & input$size <= 500, ' For this visualization, Shiny has set a maximum of 500 trials and a minimum of 1.')
    )
    # plot the binomial distribution using the gbinom2 function
    if(input$regions==TRUE){
      #print(input$cut1)
      #print(class(input$cut1))
      #print(input$cut2)
      #print(class(input$cut2))
      if (!is.na(input$cut1)){
        validate(
          need(is.wholenumber(input$cut1), 'Shiny would like you to enter whole numbers for the cutpoint.')
        )
          validate(
            need(input$cut1 >=0 & input$cut1 <= input$size, 'Shiny finds that you have entered a cutpoint too large, or less than 0.')
          )}
      if (!is.na(input$cut2)){
        validate(
          need(is.wholenumber(input$cut2), 'Shiny would like you to enter whole numbers for the cutpoint.')
        )
        validate(
          need(input$cut2 >=0 & input$cut2 <= input$size, 'Shiny finds that you have entered a cutpoint too large, or less than 0.')
        )}
      gbinom2(p=input$pr,n=input$size, scale=F,a=input$cut1,b=input$cut2)
    }
    if(input$regions==FALSE){
      gbinom2(p=input$pr,n=input$size, scale=!input$scale2,a=NA,b=NA)
    }
  }
  
  output$distPlot <- renderPlot({
    makePlot()
    },width=600)

#  output$about <- renderRmd('about.Rmd', input)
  
  output$tab1 <- renderTable({
    Successes <- 0:input$size
    #print(dbinom(Successes,size=input$size,prob=input$pr))
    Probability <- pdisplay(dbinom(Successes,size=input$size,prob=input$pr),digits.scientific=4)
#    Probability <- dbinom(Successes,size=input$size,prob=input$pr)
    tab1.df <- as.data.frame(cbind(Successes,Probability))
#    tab1.df$Successes <- as.factor(tab1.df$Successes)
    #print(tab1.df)
  },digits=c(0,0,5),include.rownames=FALSE)


  
  output$downloadPlotpdf <- downloadHandler(
    filename = function() { 
#      paste0("test1.pdf") 
      paste0(format(Sys.time(), "%Y-%m-%d:%T-%p"),round(runif(1,1,200),0), ".pdf")
      },
      content = function(file1) {
      CairoPDF(file1)
      makePlot()
      dev.off()
#      if (file.exists(paste0(file1, ".pdf")))
#        file.rename(paste0(file1, ".pdf"), file1)
    })        
  
  output$downloadPlotpng <- downloadHandler(
    filename = function() { 
#      paste0("test1.png") 
      paste0(format(Sys.time(), "%Y-%m-%d:%T-%p"),round(runif(1,1,200),0), ".png")
      },
    content = function(file1) {
      png(file1)
      makePlot()
      dev.off()
    })
})
