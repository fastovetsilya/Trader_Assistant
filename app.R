library(shiny)
library(shinythemes)
library(quantmod)

# Define UI
ui <- fluidPage(theme = shinytheme('sandstone'), 
  titlePanel('Stock price monitor'),
  
  sidebarLayout(
    # Define inputs
    sidebarPanel(
      textInput(inputId = 'ticker', label = 'Ticker', value = 'GOOGL'),
      dateRangeInput(inputId = 'dates', label = 'Dates', 
                     max = Sys.Date(),
                     start = Sys.Date() - 365,
                     end = Sys.Date()), 
      selectInput(inputId = 'source', label = 'Data Source', 
                  choices = c('google', 'yahoo', 'FRED', 'Oanda'), 
                  selected = 'yahoo')
    ),
  
  # Define output rendering
  mainPanel(
    plotOutput(outputId = 'candlechart')
  )))
  

# Define Server
server <- function(input, output)
{
  output$candlechart <- renderPlot({
    stock_ticker <- input$ticker
    data_source <- input$source
    stockEnv <- new.env()
    stock_symbol <- getSymbols(Symbols = stock_ticker, src = data_source, env = stockEnv)
    stock_dataframe <- stockEnv[[stock_symbol]]
    date_from <- input$dates[1]
    date_to <- input$dates[2]
    expr = candleChart(stock_dataframe, name = paste0(stock_symbol), subset=paste0(date_from, '::', date_to), theme = 'white')
  })
}

shinyApp(ui=ui, server = server)