#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyquant)
library(ggplot2)
library(quantmod)
# Define UI
ui <- fluidPage(
  titlePanel("Stock Price App"),
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "ticker",
        label = "Ticker Symbol",
        choices = c("AAPL", "AMZN", "GOOGL", "MSFT", "TSLA","000001.SZ"),
        selected = "AAPL"
      ),
      radioButtons(
        inputId = "period",
        label = "Time Period",
        choices = c("5 day" = 5, "10 days" = 10, "1 month" = 30, "3 months" = 90, "1 year" = 365),
        selected = 5
      ),
      selectInput(
        inputId = "plot_type",
        label = "Plot Type",
        choices = c("Line" = "line", "Bars" = "bars", "Candlesticks" = "candlesticks", "Matchsticks"="matchsticks"),
        selected = "candlesticks"
      )
    ),
    mainPanel(
      plotOutput(outputId = "plot"),
      tableOutput(outputId = "data")
    )
  )
)
# Define server
server <- function(input, output) {
  output$plot <- renderPlot({
    library(tidyverse)
    library(plotly)
    
    # Get data
   # ticker_data <<- tq_get(input$ticker,
              #             from = Sys.Date() - days(input$period),
              #             to = Sys.Date())
    
    ticker_data <<-  reactive({
                     getSymbols(input$ticker,
                           from = Sys.Date() - days(input$period),
                           to = Sys.Date(), auto.assign = FALSE)
                                                     })
    # Plot data
    plot_type <- switch(input$plot_type,
                        "line" = "line",
                        "bar" = "bars",
                        "candlesticks" = "candlesticks",
                        "matchsticks" = "matchsticks")
    
    chartSeries(ticker_data(),type = plot_type, name= input$ticker, 
                bar.type = "ohlc",theme = "white")
    
   # ggplot(ticker_data, aes(x = date, y = close)) +
     # geom_(color = "black") +
    #  labs(title = input$ticker, x = "Date", y = "Price")
  })
 
  # Create a table of the stock data
  output$data <- renderTable(data.frame(ticker_data()), spacing = "xs", rownames= TRUE, 
                             striped=TRUE, hover=TRUE, nrow= 10)
}

# Run app
shinyApp(ui = ui, server = server)