library(shinydashboard)
library(flexdashboard)
library(shiny)
library(gsheet)
library(tidyverse)
library(lubridate)
library(scales)
library(DT)
library(formattable)
library(plotly)

theme_set(theme_minimal(base_size = 18))

dat <<-
  gsheet2tbl(
    'docs.google.com/spreadsheets/d/12p_yT8VF34wybsTxsta85TjpbmhA9tSwXKgYJSaVdpk/edit?usp=sharing'
  )
names(dat)

source(
  "https://raw.githubusercontent.com/nzcoops/r-code/master/modified_mortgage_calculator.R"
)

dat2 <<- dat %>%
  select(DATE:`TOTAL PAYMENT`) %>%
  mutate(
    date = dmy(DATE),
    bal = parse_number(`ACTUAL BALANCE`),
    add = parse_number(`ADDITIONAL PAYMENT`),
    int = parse_number(INTEREST),
    off = parse_number(`OFFSET SAVING`),
    tot = parse_number(`TOTAL PAYMENT`),
    dateNum = as.numeric(date),
    dateNum = dateNum - ((2012 - 1970) * 365),
    add = ifelse(is.na(add), 0, add)
    #add = case_when(add == NA_real_ ~ 0,
    #                 TRUE ~ as.numeric(add))
  ) %>%
  filter(!is.na(date),!is.na(bal)) %>%
  #group_by(date) %>%
  arrange(date) %>%
  mutate(drop = lag(bal) - bal) %>%
  ungroup() %>%
  select(date:drop)

mine <- mortgage(dat2$bal[1], 5, 30)


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
        breaks = seq(12, plotD$month[nrow(plotD)], 36),
        labels = seq(12, plotD$month[nrow(plotD)], 36) / 12
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
      max = currency(sum(aDFmonth$Monthly_Interest)),
      gaugeSectors(
        success = c(100000, currency(sum(
          aDFmonth$Monthly_Interest
        ))),
        warning = c(50001, 100000),
        danger = c(0, 50000)
      )
    )
  })
})