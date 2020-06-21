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
                  selected = 'yahoo'),
      selectInput(inputId = 'plottype', label = 'Plot type', 
                  choices = c('line', 'candlesticks', 'matchsticks', 'bars'), 
                  selected = 'candlesticks'),
      checkboxGroupInput(inputId = 'indicators', label = 'Add indicators', 
                         choices = c('Simple Moving Average (SMA)',
                                     'Exponential Moving Average (EMA)', 
                                     'Double Exponential Moving Average (DEMA)', 
                                     'Weighted Moving Average (WMA)', 
                                     'Exponential Volume Weigthed Moving Average (EVWMA)',
                                     'Moving Average Convergence Divergence (MACD)', 
                                     'Welles Wilder\'s Directional Movement Indicator (ADX)', 
                                     'Average True Range (ATR)', 
                                     'Bollinger Bands', 
                                     'Commodity Channel Index (CCI)', 
                                     'Chaiken Money Flow (CMF)', 
                                     'Chande Momentum Oscillator (CMO)', 
                                     'Detrended Price Oscillator (DPO)', 
                                     'Price Envelope', 
                                     'Options and Futures Expiration',
                                     'Momentum', 
                                     'Rate of Change (ROC)', 
                                     'Relative Strength Indicator (RSI)', 
                                     'Parabolic Stop and Reverse (SAR)', 
                                     'Stocastic Momentum Index (SMI)', 
                                     'Triple Smoothed Exponential Oscillator (TRIX)',
                                     'Williams %R', 
                                     'ZLEMA'
                                     ))
    ),
  
  # Define output rendering
  mainPanel(
    plotOutput(outputId = 'candlechart'),
    plotOutput(outputId = 'chartseries')
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
    plot_type <- input$plottype
    chartSeries(stock_dataframe, name = paste0(stock_symbol), subset=paste0(date_from, '::', date_to), 
                theme = 'white', type = plot_type)
  
    # Add inticators
    #Welles Wilder's Directional Movement Indicator
    addADX()
    #Average True Range
    addATR()
    #Bollinger Bands
    addBBands()
    #Commodity Channel Index
    addCCI()
    #Chaiken Money Flow
    addCMF()
    #Chande Momentum Oscillator
    addCMO()
    #Double Exponential Moving Average
    addDEMA()
    #Detrended Price Oscillator
    addDPO()
    #Exponential Moving Average
    addEMA()
    #Price Envelope
    addEnvelope()
    #Exponential Volume Weigthed Moving Average
    addEVWMA()
    #Options and Futures Expiration
    addExpiry()
    #Moving Average Convergence Divergence
    addMACD()
    #Momentum
    addMomentum()
    #Rate of Change
    addROC()
    #Relative Strength Indicator
    addRSI()
    #Parabolic Stop and Reverse
    addSAR()
    #Simple Moving Average
    addSMA()
    #Stocastic Momentum Index
    addSMI()
    #Triple Smoothed Exponential Oscillator
    addTRIX()
    #Weighted Moving Average
    addWMA()
    #Williams %R
    addWPR()
    #ZLEMA
    addZLEMA()
    
  })
    
}

shinyApp(ui=ui, server = server)