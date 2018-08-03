#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# FFS http://gkoberger.github.io/Font-Awesome/icons/ not https://fontawesome.com/
# can use https://getbootstrap.com/docs/3.3/components/




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

dat <<- gsheet2tbl('docs.google.com/spreadsheets/d/12p_yT8VF34wybsTxsta85TjpbmhA9tSwXKgYJSaVdpk/edit?usp=sharing')
names(dat)

#source("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/mortgage.R")
source("https://raw.githubusercontent.com/nzcoops/r-code/master/modified_mortgage_calculator.R")

dat2 <<- dat %>%
  select(DATE:`TOTAL PAYMENT`) %>%
  mutate(date = dmy(DATE),
         bal = parse_number(`ACTUAL BALANCE`),
         add = parse_number(`ADDITIONAL PAYMENT`),
         int = parse_number(INTEREST),
         off = parse_number(`OFFSET SAVING`),
         tot = parse_number(`TOTAL PAYMENT`),
         dateNum = as.numeric(date),
         dateNum = dateNum - ((2012-1970)*365),
         add = ifelse(is.na(add), 0, add)
         #add = case_when(add == NA_real_ ~ 0,
        #                 TRUE ~ as.numeric(add))
  ) %>%
  filter(!is.na(date), !is.na(bal)) %>%
  #group_by(date) %>%
  arrange(date) %>%
  mutate(drop = lag(bal) - bal) %>%
  ungroup() %>%
  select(date:drop)

mine <- mortgage(dat2$bal[1], 5, 30)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th")),
      menuItem("Calculator", tabName = "calculator", icon = icon("bar-chart-o")),
      
      #menuItem("Calculator", tabName = "calculator", icon = icon("bar-chart-o"),
      #         menuItem("Basic Calculator", tabName = "bascal", 
      ##         menuSubItem("Principle", numericInput("uprin", "ipal", 
      #                      value = 300000, min = 50000, max = 2000000, step = 10000))),
      #        menuItem("Advanced Calculator",
                       #   "Dashboard", tabName = "dashboard", icon = icon("calendar")
                       #),
      #                 menuSubItem("Principal", numericInput("auprin", "Princ",
                                                
      #                                          value = 300000, min = 50000, max = 2000000, step = 10000)))),
      
      # menuItem("Calculator2", tabName = "calculator2", icon = icon("bar-chart-o"),
      #          menuItem("Basic Calculator",
      #                   #   "Dashboard", tabName = "dashboard", icon = icon("calendar")
      #                   #),
      #                   menuSubItem(numericInput("uprin",
      #                                            h4("Principal"), 
      #                                            value = 300000, min = 50000, max = 2000000, step = 10000))),
      #          menuItem("Advanced Calculator",
      #                   #   "Dashboard", tabName = "dashboard", icon = icon("calendar")
      #                   #),
      #                   numericInput("auprin", "Prin",
      #                   value = 300000, min = 50000, max = 2000000, step = 10000))),
               #),#,
               #numericInput("uir", 
              #              h3("Interest Rate"), 
              #              value = 4, min = 1, max = 10, step = 0.25),
              # numericInput("udur", 
              #              h3("Duration"), 
              #              value = 30, min = 5, max = 40, step = 1)) 
               
               
               #menuSubItem("Sub-item 2", tabName = "subitem2")
      menuItem("DBSCAN", tabName = "dbscan")
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1"), width = 400)
              ),
              
              fluidRow(
                box(plotOutput("plot2"), width = 400)
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content"),
              
              fluidRow(
                # A static infoBox
                infoBox("Initial Principal", currency(dat2$bal[1]), icon = icon("credit-card"), width = 4),
                infoBox("Principal Remaining", currency(dat2$bal[nrow(dat2)]) , icon = icon("calendar"), width = 4),
                infoBox("Principal Paid", currency(dat2$bal[1] - dat2$bal[nrow(dat2)]) , icon = icon("calendar"), width = 4)
              ),
              fluidRow(
                infoBox("~Total Interest", currency(sum(mine$month$Monthly_Interest)) , icon = icon("euro", lib = "glyphicon"), width = 4),
                infoBox("Interest Remaining", currency(sum(mine$month$Monthly_Interest) - sum(dat2$int)) , icon = icon("euro", lib = "glyphicon"), width = 4),
                infoBox("Interest Paid", currency(sum(dat2$int)) , icon = icon("money-bill", "fa-3x", lib = "font-awesome"), width = 4)
              )
                ,
              fluidRow(box(flexdashboard::gaugeOutput("pp"), width = 6, title="Principal Progress", background ="green"),
                       box(flexdashboard::gaugeOutput("ip"), width = 6, title="Interest Progress", background ="green")
              ),
              fluidRow(
                infoBox("Offset Savings", currency(sum(dat2$off)) , icon = icon("euro", lib = "glyphicon"), width = 4),
                infoBox("Max Principal Drop", currency(max(dat2$drop - dat2$add, na.rm = T)) , icon = icon("euro", lib = "glyphicon"), width = 4),
                infoBox("Minimum Monthy Payment", currency(min(dat2$tot)) , icon = icon("money-bill", "fa-3x", lib = "font-awesome"), width = 4)
              )
      ),
      # Third tab content
      tabItem(tabName = "calculator",
              h2("Widgets tab content"),
              
              fluidRow(box(numericInput("auprin", "Prin", value = 300000, min = 50000, max = 2000000, step = 10000), width = 4),
                       box(numericInput("uir", "Interest Rate", value = 4, min = 1, max = 10, step = 0.25), width = 4),
                       box(numericInput("udur", "Duration", value = 30, min = 5, max = 40, step = 1), width = 4) 
              ),
              fluidRow(box(plotOutput("plot3"), width = 12)
              ),
              fluidRow(box(plotOutput("plot4"), width = 12)
              ),
              fluidRow(
                infoBoxOutput('infoBox1'),
                infoBoxOutput('infoBox2'),
                infoBox("Principal Paid", currency(dat2$bal[1] - dat2$bal[nrow(dat2)]) , icon = icon("calendar"), width = 4)
              )
              
      ),
      #tabItem("calculator","Tab name calculator"),
      tabItem("bascal","Tab name bascal"),
      tabItem("Prin","Tab name Prin"),
      tabItem("dbscan","Tab name dbscan")#,
      # tabItem("calculator", tabName = "SAY WHAT",
      #         fluidRow(
      #           box(plotOutput("plot2"), width = 400)
      #         ),
      #         
      #         fluidRow(
      #           box(plotOutput("plot1"), width = 400)
      #         )
      # )
    )
  )
)
  

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    p <- ggplot(dat2, aes(x=date, y = bal)) +
      geom_line() +
      coord_cartesian(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
                      y = c(0, max(dat2$bal) * 1.2)) +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "Calendar Year", y = "Balance of Prinipal ($)", title = "Some title")
    #print(ggplotly(p))
    p
  })
  
  output$plot2 <- renderPlot({
    ggplot(dat2, aes(x=date, y = int)) +
      geom_line() +
      coord_cartesian(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
                      y = c(0, max(dat2$int) * 1.2)) +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "Calendar Year", y = "Balance of Prinipal ($)", title = "Some title")
  })
  
  calc_mor <- reactive({mortgage(input$auprin, input$uir, input$udur)})
  
  output$infoBox1 <- renderInfoBox({
    infoBox("Total Repayment", currency(sum(calc_mor()$month$Monthly_Payment)), icon = icon("credit-card"), width = 4)
  })
  
  output$infoBox2 <- renderInfoBox({
    infoBox("Interest To Pay", currency(sum(calc_mor()$month$Monthly_Interest)), icon = icon("credit-card"), width = 4)
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
    
    ggplot(plotPrep(), aes(x = month, y = Value, fill = Payment, group = Payment)) +
      geom_area(position = "stack") +
      #coord_cartesian(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
      #                y = c(0, max(dat2$int) * 1.2)) +
      scale_y_continuous(labels = dollar_format()) +
      scale_x_continuous(breaks = seq(12,plotD$month[nrow(plotD)], 36), labels = seq(12,plotD$month[nrow(plotD)], 36) / 12) +
      labs(x = "Calendar Year", y = "Balance of Prinipal ($)", title = "Some title")
  })
  
  output$pp <- renderGauge({
    gauge(currency(dat2$bal[nrow(dat2)]), min = 0, max = currency(dat2$bal[1]), gaugeSectors(
      success = c(0, 100000), 
      warning = c(100001, 200000), 
      danger = c(200001, currency(dat2$bal[1]))
    ))
  })
  
  output$ip <- renderGauge({
    gauge(currency(sum(dat2$int)), min = 0, max = currency(sum(aDFmonth$Monthly_Interest)), gaugeSectors(
      success = c(100000, currency(sum(aDFmonth$Monthly_Interest))), 
      warning = c(50001, 100000), 
      danger = c(0, 50000)
    ))
  })
}

shinyApp(ui, server)