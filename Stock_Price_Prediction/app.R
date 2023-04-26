#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
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
        choices = c("AAPL", "AMZN", "GOOGL", "MSFT", "TSLA"),
        selected = "AAPL"
      ),
      radioButtons(
        inputId = "period",
        label = "Time Period",
        choices = c("1 day" = 1, "5 days" = 5, "1 month" = 30, "3 months" = 90, "1 year" = 365),
        selected = 1
      ),
      selectInput(
        inputId = "plot_type",
        label = "Plot Type",
        choices = c("Line" = "line", "Bar" = "bar", "Candlestick" = "candlestick"),
        selected = "line"
      )
    ),
    mainPanel(
      plotOutput(outputId = "plot"),
      plotOutput(outputId = "data")
    )
  )
)

# Define server
server <- function(input, output) {
  output$plot <- renderPlot({
    library(tidyverse)
    library(plotly)
    
    # Get data
    data <- tq_get(input$ticker,
                   from = Sys.Date() - days(input$period),
                   to = Sys.Date())
    
    # Plot data
    plot_type <- switch(input$plot_type,
                        "line" = line,
                        "bar" = bar,
                        "candlestick" = candlestick)
    
    ggplot(data, aes(x = date, y = close)) +
      geom_line(color = "black") +
      labs(title = input$ticker, x = "Date", y = "Price")
  })
  
  # Create a table of the stock data
  output$data <- renderTable({
    data.frame(data)
  })
}

# Run app
shinyApp(ui = ui, server = server)