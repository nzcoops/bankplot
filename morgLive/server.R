
shinyServer(function(input, output) {
  output$plot1 <- renderPlot({
    p <- ggplot(dat2, aes(x = date, y = bal)) +
      geom_line() +
      coord_cartesian(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
                      y = c(0, max(dat2$bal) * 1.2)) +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "Calendar Year", y = "Balance of Prinipal ($)", title = "Some title")
    #print(ggplotly(p))
    p
  })
  
  output$plot2 <- renderPlot({
    ggplot(dat2, aes(x = date, y = int)) +
      geom_line() +
      coord_cartesian(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
                      y = c(0, max(dat2$int) * 1.2)) +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "Calendar Year", y = "Balance of Prinipal ($)", title = "Some title")
  })
  
  calc_mor <-
    reactive({
      mortgage(input$auprin, input$uir, input$udur)
    })
  
  output$infoBox1 <- renderInfoBox({
    infoBox(
      "Total Repayment",
      currency(sum(calc_mor()$month$Monthly_Payment)),
      icon = icon("credit-card"),
      width = 4
    )
  })
  
  output$infoBox2 <- renderInfoBox({
    infoBox(
      "Interest To Pay",
      currency(sum(calc_mor()$month$Monthly_Interest)),
      icon = icon("credit-card"),
      width = 4
    )
  })
  
  output$plot3 <- renderPlot({
    ggplot(calc_mor()$year, aes(x = Year, y = Amortization)) +
      geom_line() +
      #coord_cartesian(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
      #                y = c(0, max(dat2$int) * 1.2)) +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "Calendar Year", y = "Balance of Prinipal ($)", title = "Some title")
  })
  
  plotPrep <- function() {
    datT <- mortgage(input$auprin, input$uir, input$udur)
    plotD <- datT$month %>%
      mutate(month = 1:n()) %>%
      dplyr::rename(Principal = Monthly_Principal,
                    Interest = Monthly_Interest) %>%
      gather(key = Payment, value = Value, Principal, Interest)
  }
  
  output$plot4 <- renderPlot({
    ggplot(plotPrep(),
           aes(
             x = month,
             y = Value,
             fill = Payment,
             group = Payment
           )) +
      geom_area(position = "stack") +
      #coord_cartesian(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
      #                y = c(0, max(dat2$int) * 1.2)) +
      scale_y_continuous(labels = dollar_format()) +
      scale_x_continuous(
        breaks = seq(12, plotPrep()$month[nrow(plotPrep())], 36),
        labels = seq(12, plotPrep()$month[nrow(plotPrep())], 36) / 12
      ) +
      labs(x = "Calendar Year", y = "Balance of Prinipal ($)", title = "Some title")
  })
  
  output$pp <- renderGauge({
    gauge(
      currency(dat2$bal[nrow(dat2)]),
      min = 0,
      max = currency(dat2$bal[1]),
      gaugeSectors(
        success = c(0, 100000),
        warning = c(100001, 200000),
        danger = c(200001, currency(dat2$bal[1]))
      )
    )
  })
  
  output$ip <- renderGauge({
    gauge(
      currency(sum(dat2$int)),
      min = 0,
      max = currency(sum(mine$month$Monthly_Interest)),
      gaugeSectors(
        success = c(100000, currency(sum(
          mine$month$Monthly_Interest
        ))),
        warning = c(50001, 100000),
        danger = c(0, 50000)
      )
    )
  })
})