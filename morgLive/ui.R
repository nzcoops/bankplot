shinyUI(dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem("Widgets", tabName = "widgets", icon = icon("th")),
    menuItem(
      "Calculator",
      tabName = "calculator",
      icon = icon("bar-chart-o")
    )
  )),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(box(
                plotOutput("plot1"), width = 400
              )),
              
              fluidRow(box(
                plotOutput("plot2"), width = 400
              ))),
      
      # Second tab content
      tabItem(
        tabName = "widgets",
        h2("Widgets tab content"),
        
        fluidRow(
          # A static infoBox
          infoBox(
            "Initial Principal",
            currency(dat2$bal[1]),
            icon = icon("credit-card"),
            width = 4
          ),
          infoBox(
            "Principal Remaining",
            currency(dat2$bal[nrow(dat2)]) ,
            icon = icon("calendar"),
            width = 4
          ),
          infoBox(
            "Principal Paid",
            currency(dat2$bal[1] - dat2$bal[nrow(dat2)]) ,
            icon = icon("calendar"),
            width = 4
          )
        ),
        fluidRow(
          infoBox(
            "~Total Interest",
            currency(sum(mine$month$Monthly_Interest)) ,
            icon = icon("euro", lib = "glyphicon"),
            width = 4
          ),
          infoBox(
            "Interest Remaining",
            currency(sum(mine$month$Monthly_Interest) - sum(dat2$int)) ,
            icon = icon("euro", lib = "glyphicon"),
            width = 4
          ),
          infoBox(
            "Interest Paid",
            currency(sum(dat2$int)) ,
            icon = icon("money-bill", "fa-3x", lib = "font-awesome"),
            width = 4
          )
        )
        ,
        fluidRow(
          box(
            flexdashboard::gaugeOutput("pp"),
            width = 6,
            title = "Principal Progress",
            background = "green"
          ),
          box(
            flexdashboard::gaugeOutput("ip"),
            width = 6,
            title = "Interest Progress",
            background = "green"
          )
        ),
        fluidRow(
          infoBox(
            "Offset Savings",
            currency(sum(dat2$off)) ,
            icon = icon("euro", lib = "glyphicon"),
            width = 4
          ),
          infoBox(
            "Max Principal Drop",
            currency(max(dat2$drop - dat2$add, na.rm = T)) ,
            icon = icon("euro", lib = "glyphicon"),
            width = 4
          ),
          infoBox(
            "Minimum Monthy Payment",
            currency(min(dat2$tot)) ,
            icon = icon("money-bill", "fa-3x", lib = "font-awesome"),
            width = 4
          )
        )
      ),
      # Third tab content
      tabItem(
        tabName = "calculator",
        h2("Widgets tab content"),
        
        fluidRow(
          box(
            numericInput(
              "auprin",
              "Prin",
              value = 300000,
              min = 50000,
              max = 2000000,
              step = 10000
            ),
            width = 4
          ),
          box(
            numericInput(
              "uir",
              "Interest Rate",
              value = 4,
              min = 1,
              max = 10,
              step = 0.25
            ),
            width = 4
          ),
          box(
            numericInput(
              "udur",
              "Duration",
              value = 30,
              min = 5,
              max = 40,
              step = 1
            ),
            width = 4
          )
        ),
        fluidRow(box(plotOutput("plot3"), width = 12)),
        fluidRow(box(plotOutput("plot4"), width = 12)),
        fluidRow(
          infoBoxOutput('infoBox1'),
          infoBoxOutput('infoBox2'),
          infoBox(
            "Principal Paid",
            currency(dat2$bal[1] - dat2$bal[nrow(dat2)]) ,
            icon = icon("calendar"),
            width = 4
          )
        )
        
      ),
      #tabItem("calculator","Tab name calculator"),
      tabItem("bascal", "Tab name bascal"),
      tabItem("Prin", "Tab name Prin"),
      tabItem("dbscan", "Tab name dbscan")#,
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
))
