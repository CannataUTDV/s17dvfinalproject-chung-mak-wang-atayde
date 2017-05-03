require(shiny)
require(shinydashboard)
require(DT)
require(leaflet)
require(plotly)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Box Plot", tabName = "boxplot", icon = icon("dashboard")),
      menuItem("Histogram", tabName = "histogram", icon = icon("dashboard")),
      menuItem("Scatter Plot", tabName = "scatter", icon = icon("dashboard")),
      menuItem("Crosstab with KPI", tabName = "crosstab", icon = icon("dashboard")),
      menuItem("Barchart", tabName = "barchart", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),    
    tabItems(
      # Begin Box Plots tab content.
      tabItem(tabName = "boxplot",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb5", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         uiOutput("boxplotDays"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html,
                         actionButton(inputId = "click5",  label = "To get data, click here"),
                         DT::dataTableOutput("boxplotData1")
                ),
                tabPanel("Box Plot", plotlyOutput("boxplotPlot1", height=1000))
              )
      ),
      # End Box Plots tab content.
      # Begin Histogram tab content.
      tabItem(tabName = "histogram",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb4", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         actionButton(inputId = "click4",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("histogramData1")
                ),
                tabPanel("Histogram", plotlyOutput("histogramPlot1", height=1000))
              )
      ),
      # End Histograms tab content.
      # Begin Scatter Plots tab content.
      tabItem(tabName = "scatter",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb3", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         uiOutput("scatterStates"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html,
                         actionButton(inputId = "click3",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("scatterData1")
                ),
                tabPanel("Scatter Plot", plotlyOutput("scatterPlot1", height=1000))
              )
      ),
      # End Scatter Plots tab content.
      # Begin Crosstab tab content.
      tabItem(tabName = "crosstab",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb1", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         sliderInput("KPI1", "KPI_Low:", 
                                     min = .5, max = 1,  value = .1),
                         sliderInput("KPI2", "KPI_Medium:", 
                                     min = 1, max = 2,  value = .1),
                         actionButton(inputId = "click1",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data1")
                ),
                tabPanel("Crosstab", plotOutput("plot1", height=1000))
              )
      ),
      # End Crosstab tab content.
      # Begin Barchart tab content.
      tabItem(tabName = "barchart",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb2", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         uiOutput("regions2"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                         actionButton(inputId = "click2",  label = "To get data, click here"),
                         'Here is data for the "Barchart" tab', 
                         hr(), 
                         DT::dataTableOutput("barchartData1")
                ),
                tabPanel("Barchart", plotOutput("barchartPlot1", height=500))
              )
      )
      # End Barchart tab content.
    )
  )
)