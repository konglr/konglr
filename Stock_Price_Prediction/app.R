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
  titlePanel("Stock Price Prediction-AI股票价格预测"),
  sidebarLayout(
    sidebarPanel(width = 3,
      pickerInput(
        inputId = "ticker",
        label = "Ticker Symbol",
        choices = c("AAPL", "AMZN", "GOOGL", "MSFT", "TSLA","000001.SZ"),
        selected = "AAPL"
      ),
      radioButtons(
        inputId = "period",
        label = "Time Period",
        choices = c("10 day" = 10, "20 days" = 20, "1 month" = 30, "3 months" = 90, "1 year" = 365),
        selected = 10
      ),
      selectInput(
        inputId = "plot_type",
        label = "Plot Type",
        choices = c("Line" = "line", "Bars" = "bars", "Candlesticks" = "candlesticks", "Matchsticks"="matchsticks"),
        selected = "candlesticks"
      ),
      
      radioButtons("radio", label = "Prediction Models",
                   choices = list("Keras-5 days back" = 1, "Karas-SMA" = 2, "Keras-RSI" = 3), 
                   selected = 1)
    ),
    mainPanel(width = 9,
      plotOutput(outputId = "plot"),
      tableOutput(outputId = "data"),
      textOutput("prediction_result")
    )
  )
)
# Define server
server <- function(input, output) {
  library(tidyverse)
  library(plotly)
  
  # Get data
  ticker_data <<-  reactive({
    getSymbols(input$ticker,
               from = Sys.Date() - days(input$period),
               to = Sys.Date(), auto.assign = FALSE)
  })
  
  output$plot <- renderPlot({
  
    # Plot data
    plot_type <- switch(input$plot_type,
                        "line" = "line",
                        "bar" = "bars",
                        "candlesticks" = "candlesticks",
                        "matchsticks" = "matchsticks")
    
    chartSeries(ticker_data(),type = plot_type, name= input$ticker, 
                bar.type = "ohlc", plot= TRUE, theme = "white")
    
   # ggplot(ticker_data, aes(x = date, y = close)) +
     # geom_(color = "black") +
    #  labs(title = input$ticker, x = "Date", y = "Price")
  })
 
  # Create a table of the stock data
  output$data <- renderTable(data.frame(ticker_data()[tail(1:nrow(ticker_data()), 5), 1:5]), spacing = "xs", rownames= TRUE,
                             striped=TRUE, hover=TRUE)
  
  output$prediction_result <- renderText({
    prediction <- predict_result()
    
    # Format and display the prediction result
    # Example: return(paste0("Prediction: ", prediction))
  })
  
}

# Run app
shinyApp(ui = ui, server = server)