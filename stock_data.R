library(quantmod)

# Initialize ticker and data source
stock_ticker <- 'MMM'
data_source <- 'yahoo'

# Load the data
stockEnv <- new.env()
stock_symbol <- getSymbols(Symbols = stock_ticker, src = data_source, env = stockEnv)
stock_dataframe <- stockEnv[[stock_symbol]]

# Bar chart
barChart(stockEnv[[stock_symbol]], name = paste0(stock_symbol), subset='2007::2008-01', theme = 'white')

# Candle chart
candleChart(stockEnv[[stock_symbol]], name = paste0(stock_symbol), subset='2007::2008-01', theme = 'white')

# Add indicators
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
# Options and Futures Expiration
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
#Volume
addVo()
#Weighted Moving Average
addWMA()
#Williams %R
addWPR()
#ZLEMA
addZLEMA()