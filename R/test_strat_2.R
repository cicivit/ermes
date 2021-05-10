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
entry_fun <- entry_on_MAcross
entry_args <- list(fast_n=60*5, slow_n=60*24*50, fastMA_fun=TTR::EMA)
exit_fun <- exitTP_SL_time
exit_args <- list(time_hours = 72)
CloseTradeOnOppositeSignal <- TRUE
system.time(
  bt <- backtest(ask, bid, entry_fun, entry_args=list(fast_n=60*5, slow_n=60*24*50, fastMA_fun=TTR::EMA),
                 exit_fun = exit_fun,
                 exit_args <- list(time_hours = 72),
                 CloseTradeOnOppositeSignal <- TRUE)
)

plot_returns(bt)

