#library(shiny)
library(Cairo)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
source("tools.R", local=TRUE)

  makeplot <- function() {
    if (input$control == "prob") {
      if (input$entrymode == "slider") {
        if (input$tail == "upper") {
          qgnormz(pr = input$pru,tail = input$tail)
        }
        else if (input$tail == "lower") {
          qgnormz(pr = input$prl,tail = input$tail)
        }
        else if (input$tail == "two") {
          qgnormz(pr = input$prt,tail = input$tail)
        }
      }
      else if (input$entrymode == "box") {
        if (input$tail == "upper") {
          validate(
            need(input$pvalu < .999999999999999 & input$pvalu > .000000000000001, 'Shiny would like you to enter a probability above 0 and less than 1.0 \n(best usage is to no more than 5 decimal places)')
          )
          qgnormz(pr = input$pvalu,tail = input$tail)
        }
        else if (input$tail == "lower") {
          validate(
            need(input$pvall < .999999999999999 & input$pvall > .000000000000001, 'Shiny would like you to enter a probability above 0 and less than 1.0 \n(best usage is to no more than 5 decimal places)')
          )
          qgnormz(pr = input$pvall,tail = input$tail)
        }
        else if (input$tail == "two") {
          validate(
            need(input$pvalt < .999999999999999 & input$pvalt > .000000000000001, 'Shiny would like you to enter a probability above 0 and less than 1.0 \n(best usage is to no more than 5 decimal places)')
          )
          qgnormz(pr = input$pvalt,tail = input$tail)
        }
      }
    }
    
    else if (input$control == "quant") {
      if (input$entrymode == "slider") {
        if (input$tail == "upper") {
          pgnormz(quantile = input$quantileu,tail = input$tail)
        }
        else if (input$tail == "lower") {
          pgnormz(quantile = input$quantilel,tail = input$tail)
        }
        else if (input$tail == "two") {
          pgnormz(quantile = input$quantilet,tail = input$tail)
        }
      }
      if (input$entrymode == "box") {
        if (input$tail == "upper") {
          validate(
            need(input$quantileub  >= -4.2 & input$quantileub  <= 4.2, 'Shiny prefers quantile values between -4.2 and +4.2')
          )
          pgnormz(quantile = input$quantileub,tail = "upper")
        }
        else if (input$tail == "lower") {
          validate(
            need(input$quantilelb  >= -4.2 & input$quantilelb  <= 4.2, 'Shiny prefers quantile values between -4.2 and +4.2')
          )
          pgnormz(quantile = input$quantilelb,tail = "lower")
        }
        else if (input$tail == "two") {
          validate(
            need(input$quantiletb  >= 0 & input$quantiletb  <= 4.2, 'Shiny prefers quantile values between 0 and +4.2')
          )
          pgnormz(quantile = input$quantiletb,tail = "two")
        }
      }
    }
  } #end of makeplot
  
  output$distPlot <- renderPlot({
    makeplot()
  },width = 550) # end of renderPlot
  
  output$downloadPNG <- downloadHandler(
    filename = function() {
#      paste0("test1.png")
      paste0(format(Sys.time(), "%Y-%m-%d:%T-%p"),round(runif(1,1,200),0), ".png")
    },
    content = function(file1) {
      png(file1)
      makeplot()
      dev.off()
    }
  )
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
    }
  )
  
})
