options(stringsAsFactors=FALSE)
library(quantmod)
library(data.table)

ask <- readRDS('data/EUR_USD_M1_Ask.rds')
bid <- readRDS('data/EUR_USD_M1_Bid.rds')
ask <- ask['2009::2019']
bid <- bid['2009::2019']

source('R/backtester.R')
source('R/entry_functions.R')
source('R/exit_functions.R')

bt <- backtest(ask, bid,
               entry_fun=entry_on_MAcross,
               TradeTimeLimit = years(1))
plot_returns(bt)
plot_strategy(bt, candle_period_mins=1)
