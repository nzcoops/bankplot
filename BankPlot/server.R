#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source('tinker.R', local = T)
h(dat2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #source('tinker.R', local = T)
   
  output$tab1 <- renderTable(mtcars)
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$plot1 <- renderPlot({
    
    ggplot(dat2, aes(x=date, y = bal)) +
      geom_line() +
      expand_limits(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
                    y = c(0,300000)) +
      scale_y_continuous(labels = dollar_format())
    
  })
  
  output$intro <- renderUI({"Intro.Rmd"})
  
  output$text1 <- renderText({
    paste("Total Interest Paid", sum(dat2$int))
  })
  
  output$text2 <- renderText({
    paste("Total Principal Paid", sum(dat2$drop, na.rm = T))
  })
  
})
