# condition1=  highest(high,8)<Highest(high,20); # The last 8 bars do not contain the highest high (20)
# if marketposition=0 and condition1 then buy Next Bar at low[1] limit; 
# condition2=  Average(Close,20)<Average(Close,10); # SMA 20 < SMA 10
# if marketposition = 1 then  sell  next bar at close[1] limit; # Limit order to close all open buy orders
# 
# 
# if marketposition=0 and condition2 then Sellshort next bar at high[1] limit; # Limit sell at prev high price
# if marketposition<0  then Buytocover next bar at low[1] limit; # If sell order currently open, limit buy to close at prev low
# 
# # What is the timeframe for this?
# # Are we looking at the crossover of MA / Running Highs?
# 
# 
# if (max(high 8) < max(high 20) & marketpos == 0) {
#   # Set a limit buy order equal to the last bars low price
# }
# if(mktpos > 0) {
#   # enter a limit sell order at prev close price
# } 
# 
# if(Avg(Close 20) < Avg(Close10)) {
#   if(mktpos==0) {
#     # limit sell at prev high
#   }
# }
# if(mktpos < 0) {
#   # Close position (Limit at Prev Low)
# }

options(stringsAsFactors=FALSE)
library(data.table)
library(quantmod)
library(myFinanceTools)
library(stringr)
library(lubridate)
library(plotly)
source('R/utility_functions.R')
source('R/entry_functions.R')
source('R/exit_functions.R')
source('R/backtester.R')
source('R/plotting.R')

ask <- readRDS('data/EUR_USD_M1_Ask.rds')
bid <- readRDS('data/EUR_USD_M1_Bid.rds')
ask <- ask['2009::2019']
bid <- bid['2009::2019']
CloseTradeOnOppositeSignal <- FALSE

entry_function <- function(this.dat, timeframe='h1', fastHi=8, slowHi=20, fastMA=10, slowMA=20){ 
  m1 <- dt_to_xts(this.dat[, .(date, Open.a, High.a, Low.a, Close.a)])
  recentHi <- coredata(mergeHighLowTimes(m1, lgTP=timeframe, smTP='m1', cols='High', fun=runMax, n=fastHi)$out)
  pastHi <- coredata(mergeHighLowTimes(m1, lgTP=timeframe, smTP='m1', cols='High', fun=runMax, n=slowHi)$out)
  fastSMA <- coredata(mergeHighLowTimes(m1, lgTP=timeframe, smTP='m1', cols='High', fun=SMA, n=fastMA)$SMA)
  slowSMA <- coredata(mergeHighLowTimes(m1, lgTP=timeframe, smTP='m1', cols='High', fun=SMA, n=slowMA)$SMA)
  prevCl <- coredata(mergeHighLowTimes(m1, timeframe, 'm1', cols='Close', fun=quantmod::Lag, k=1 )$Lag.1)
  prevHi <- coredata(mergeHighLowTimes(m1, timeframe, 'm1', cols='High', fun=quantmod::Lag, k=1 )$Lag.1)
  prevLo <- coredata(mergeHighLowTimes(m1, timeframe, 'm1', cols='Low', fun=quantmod::Lag, k=1 )$Lag.1)
  this.dat[, recentHi:=recentHi]
  this.dat[, pastHi:=pastHi]
  this.dat[, fastSMA:=fastSMA]
  this.dat[, slowSMA:=slowSMA]
  this.dat[, prevClose:=prevCl]
  this.dat[, prevHigh:=prevHi]
  this.dat[, prevLow:=prevLo]
  this.dat[recentHi < pastHi & date == floor_date(date, unit='hour'), 
           `:=`(EntryType='Limit',
                OrderSize=10000,
                EntryPrice=prevLow,
                EntryTime=date,
                TakeProfit=prevClose)]
  this.dat[slowSMA < fastSMA & date == floor_date(date, unit='hour'), 
           `:=`(EntryType='Limit',
                OrderSize=-10000,
                EntryPrice=prevHigh,
                EntryTime=date,
                TakeProfit=prevLow)]
  return(this.dat)
}

system.time(
bt <- backtest(ask, bid, entry_fun=entry_function, 
               CloseTradeOnOppositeSignal=FALSE, 
               AddToPosition=FALSE,
               TradeTimeLimit=years(8))
)
plot_returns(bt)
