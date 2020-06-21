library(shiny)
library(shinythemes)
library(quantmod)

# Define UI
ui <- fluidPage(theme = shinytheme('sandstone'), 
  titlePanel('Trading monitor'),
  
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
      selectInput(inputId = 'plottheme', label = 'Plot theme', 
                  choices = c('white', 'black'), 
                  selected = 'white'),
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
    plot_theme <- input$plottheme
    indicator_list <- 'addVo()'
    
    # Add indicators
    #Simple Moving Average
    if ('Simple Moving Average (SMA)' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addSMA()')} 
    #Exponential Moving Average
    if ('Exponential Moving Average (EMA)' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addEMA()')}  
    #Double Exponential Moving Average
    if ('Double Exponential Moving Average (DEMA)' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addDEMA()')} 
    #Weighted Moving Average
    if ('Weighted Moving Average (WMA)' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addWMA()')}
    #Exponential Volume Weigthed Moving Average
    if ('Exponential Volume Weigthed Moving Average (EVWMA)' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addEVWMA()')}
    #Moving Average Convergence Divergence
    if ('Moving Average Convergence Divergence (MACD)' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addMACD()')}
    #Welles Wilder's Directional Movement Indicator
    if ('Welles Wilder\'s Directional Movement Indicator (ADX)' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addADX()')}
    #Average True Range
    if ('Average True Range (ATR)' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addATR()')}
    #Bollinger Bands
    if ('Bollinger Bands' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addBBands()')}
    #Commodity Channel Index
    if ('Commodity Channel Index (CCI)' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addCCI()')}
    #Chaiken Money Flow
    if ('Chaiken Money Flow (CMF)' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addCMF()')}
    #Chande Momentum Oscillator
    if ('Chande Momentum Oscillator (CMO)' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addCMO()')}
    #Detrended Price Oscillator
    if ('Detrended Price Oscillator (DPO)' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addDPO()')}
    #Price Envelope
    if ('Price Envelope' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addEnvelope()')}
    #Options and Futures Expiration
    if ('Options and Futures Expiration' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addExpiry()')}
    #Momentum
    if ('Momentum' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addMomentum()')}
    #Rate of Change
    if ('Rate of Change (ROC)' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addROC()')}
    #Relative Strength Indicator
    if ('Relative Strength Indicator (RSI)' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addRSI()')}
    #Parabolic Stop and Reverse
    if ('Parabolic Stop and Reverse (SAR)' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addSAR()')}
    #Stocastic Momentum Index
    if ('Stocastic Momentum Index (SMI)' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addSMI()')}
    #Triple Smoothed Exponential Oscillator
    if ('Triple Smoothed Exponential Oscillator (TRIX)' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addTRIX()')}
    #Williams %R
    if ('Williams %R' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addWPR()')}
    #ZLEMA
    if ('ZLEMA' %in% input$indicators) {indicator_list <- paste0(indicator_list, ';addZLEMA()')}

    chartSeries(stock_dataframe, name = paste0(stock_symbol), subset=paste0(date_from, '::', date_to),
                  theme = plot_theme, type = plot_type, TA = indicator_list)})
}

shinyApp(ui=ui, server = server)