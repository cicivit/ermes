# Buy = High > max(High, 14day) (Also close any short positions)
# Sell = Low < max(Low, 14day) (Also close any long positions)

# Stoploss = 30 day ATR * 3
# Takeprofit = None

# Close Entire Long positions on RSI(20 day) > 70 
# Close Short positions on RSI(20 day) < 30

# Add a limit long order 10 pips below current price when RSI(10 hour) < 30  
# Add a limit short order 10 pips above the current price RSI(10 hour) > 70 

# Close part of Long position when RSI(10h) > 70
# Close part of Short position when RSI(10h) < 30

# Have a starting balance that determines position sizes
# Every buy/sell or add to position = 2% 

options(stringsAsFactors=FALSE)
library(data.table)
library(quantmod)
library(stringr)
library(lubridate)
library(plotly)
source('R/utility_functions.R')
source('R/entry_functions.R')
source('R/exit_functions.R')
source('R/backtester.R')
source('R/plotting.R')
Sys.setenv(TZ='UTC')
ask <- readRDS('data/EUR_USD_M1_Ask.rds')
bid <- readRDS('data/EUR_USD_M1_Bid.rds')
ask <- ask['2009::2019']
bid <- bid['2009::2019']


entry_function <- function(this.dat){ 
  # this.dat <- mergeAskBid(ask, bid) # This is the data the backtester uses, run this to start building your entry points
  m1 <- dt_to_xts(this.dat[, .(date, Open.a, High.a, Low.a, Close.a)])
  
  # Add all our indicatiors to m1 data
  #-------------------------------------------------------------------------------------------------
  # Buy = High > max(High, 14day) (Also close any short positions)
  lastHi <- coredata(mergeHighLowTimes(m1, lgTP='d', smTP='m1', cols='High', fun=runMax, n=14)$out)
  
  # Sell = Low < max(Low, 14day) (Also close any long positions)
  lastLo <- coredata(mergeHighLowTimes(m1, lgTP='d', smTP='m1', cols='Low', fun=runMin, n=14)$out)
  
  # Stoploss = daily ATR * 3
  atr <- coredata(mergeHighLowTimes(m1, lgTP='d', smTP='m1', cols=c('High', 'Low', 'Close'), fun=ATR, n=30)$atr)
  
  # Close long positions on RSI(20 day) > 70
  # Close Short positions on RSI(20 day) < 30
  rsi20day <- coredata(mergeHighLowTimes(m1, 'd', 'm1', cols='Close', fun=RSI, k=20)$rsi)
  
  # Add to long positions RSI(10 hour) < 30  (Close .5 of position)
  # Add to short positions RSI(10 hour) > 70 (Close .5 of position)
  rsi10hr <- coredata(mergeHighLowTimes(m1, 'h1', 'm1', cols='Close', fun=RSI, k=10)$rsi)
  
  this.dat[, `:=`(maxHi=lastHi, 
                  minLo=lastLo,
                  dailyATR=atr,
                  rsi20=rsi20day,
                  rsi10h =rsi10hr)]
  # ----------------- Indicators have been added ---------------------------------------------------
  
  # ---------------------- Market Entry ------------------------------------------------------------
  # Buy Entry Signal High crosses over max high (Only the first time in a day)
  this.dat[, Day:=as.Date(date)]
  this.dat[High.b > maxHi & shift(High.b) <= maxHi, OrderSize:=10000]
  
  # Only the first of the day
  this.dat[!is.na(OrderSize), n_highs := 1:.N, by=Day]
  this.dat[n_highs != 1, OrderSize:=NA]
  
  # Sell Entries on a new 20-day low
  this.dat[Low.a < minLo & shift(Low.a) >= minLo, OrderSize:=-10000]
  
  # Only the first of the day
  this.dat[OrderSize < 0, n_low := 1:.N, by=Day]
  this.dat[, Day:=NULL]
  this.dat[OrderSize < 0 & n_low != 1, OrderSize:=NA]
  
  this.dat[, c('n_low', 'n_highs') := NULL]
  
  # Additional info for our buy orders
  this.dat[OrderSize > 0, `:=`(EntryType='Market',
                               EntryTime=date,
                               EntryPrice=maxHi,
                               StopLoss=maxHi - dailyATR)]
  # For sell orders 
  this.dat[OrderSize < 0, `:=`(EntryType='Market',
                               EntryTime=date,
                               EntryPrice=minLo,
                               StopLoss=minLo + dailyATR)]
  # Remove any orders that don't have a StopLoss (aka no atr data)
  this.dat[is.na(dailyATR) | is.na(rsi20), OrderSize:=0]
  # ------------------------------------------------------------------------------------------------
  
  return(this.dat)
}

exit_fun <- function(this.dat, partialCloseAmount=2000, addToPositionAmount=1000) {
  # We already have the RSI 10-hour from the entry function, but we could also create it here
  
  # First get the exits of StopLoss or TakeProfit (if we had a TP), this will be the max that the trade is open
  exitSL <- exitTP_SL(this.dat)
  this.dat[, CurrentPositionSize:=OrderSize]
  newExitCols <- names(exitSL)[!names(exitSL) %in% names(this.dat)]
  
  # Remove the dates from our original data where we now have exit information and rbind our exit info
  this.dat <- this.dat[date < exitSL$date]
  this.dat <- rbind(this.dat, exitSL, fill=TRUE)
  this.dat[, LimitEntryTime:=as.POSIXct(NA)]
  side <- first(this.dat$Side)
  if(side == 1) {
    # Limit Order when rsi10 < 30
    this.dat[rsi10h < 30 & shift(rsi10h) > 30, 
             `:=`(LimitOrderPlacedTime=date,
                  LimitOrderPrice=Close.a-.0010, 
                  LimitOrderSize=addToPositionAmount*side,
                  LimitOrderID=paste0(Order_ID, '_', .I))]  
    # Fill in NA's after each value with the limit order info
    this.dat[, `:=`(LimitOrderPlacedTime=na.locf(LimitOrderPlacedTime, na.rm=FALSE),
                    LimitOrderPrice=na.locf(LimitOrderPrice, na.rm=FALSE),
                    LimitOrderSize=na.locf(LimitOrderSize, na.rm=FALSE),
                    LimitOrderID=na.locf(LimitOrderID, na.rm=FALSE))]
    
    # Get the Entry date for our limit order
    entryDates <- this.dat[date > LimitOrderPlacedTime & Low.a < LimitOrderPrice, 
                           list(LimitEntryDate=first(date)), 
                           by=LimitOrderID]
    # Increase the overall position size anytime after these dates
    if(nrow(entryDates) != 0) {
      for(iter in 1:nrow(entryDates)) {
        this.date <- entryDates$LimitEntryDate[iter]
        this.dat[date == this.date, LimitEntryTime:=this.date]
        this.dat[date >= this.date, CurrentPositionSize:=CurrentPositionSize + addToPositionAmount*side]
      }
    }
    
    # Partially close positions when RSI 10h > 70
    closeDates <- this.dat[rsi10h > 70 & shift(rsi10h) < 70, date]
    if(length(closeDates != 0)) {
      for(iter in 1:length(closeDates)) {
        this.date <- closeDates[iter]
        # Anytime we partially close a position log the exit info
        this.dat[date == this.date, 
                 `:=`(ExitTime=date,
                      ExitPrice=Close.b,
                      ExitAmount= -1 * side * partialCloseAmount,
                      ExitReason='RSI > 70, Partial Close',
                      OrigOrder=Order_ID)]
        # Reduce the order sizes of subsequent rows by this amount
        this.dat[date >= this.date, CurrentPositionSize:=CurrentPositionSize - partialCloseAmount*side]
      }
    }
    positionFullyClosedDate <- this.dat[CurrentPositionSize <= 0, first(date)]
    if(length(positionFullyClosedDate) != 0) {
      this.dat <- this.dat[date <= positionFullyClosedDate]
    }
  } else if(side == -1) {
    # Open a limit order
    this.dat[rsi10h > 70 & shift(rsi10h) < 70, 
             `:=`(LimitOrderPlacedTime=date,
                  LimitOrderPrice=Close.a+.0010, 
                  LimitOrderSize=addToPositionAmount*side,
                  LimitOrderID=paste0(Order_ID, '_', .I))]  
    # Fill in NA's after each value with the limit order info
    this.dat[, `:=`(LimitOrderPlacedTime=na.locf(LimitOrderPlacedTime, na.rm=FALSE),
                    LimitOrderPrice=na.locf(LimitOrderPrice, na.rm=FALSE),
                    LimitOrderSize=na.locf(LimitOrderSize, na.rm=FALSE),
                    LimitOrderID=na.locf(LimitOrderID, na.rm=FALSE))]
    
    # Get the Entry date for our limit order
    entryDates <- this.dat[date > LimitOrderPlacedTime & High.b > LimitOrderPrice, 
                           list(LimitEntryDate=first(date)), 
                           by=LimitOrderID]
    # Increase the overall position size anytime after these dates
    if(nrow(entryDates) !=0) {
      for(iter in 1:nrow(entryDates)) {
        this.date <- entryDates$LimitEntryDate[iter]
        this.dat[date == this.date, LimitEntryTime:=this.date]
        this.dat[date >= this.date, CurrentPositionSize:=CurrentPositionSize + addToPositionAmount*side]
      }
    }
    
    # Partially close positions when RSI 10h < 30
    closeDates <- this.dat[rsi10h < 30 & shift(rsi10h) > 30, date]
    if(length(closeDates) != 0) {
      for(iter in 1:length(closeDates)) {
        this.date <- closeDates[iter]
        # Anytime we partially close a position log the exit info
        this.dat[date == this.date, 
                 `:=`(ExitTime=date,
                      ExitPrice=Close.b,
                      ExitAmount= -1 * side * partialCloseAmount,
                      ExitReason='RSI < 30, Partial Close',
                      OrigOrder=Order_ID)]
        # Reduce the order sizes of subsequent rows by this amount
        this.dat[date >= this.date, CurrentPositionSize:=CurrentPositionSize - partialCloseAmount*side]
        this.dat[CurrentPositionSize>0, CurrentPositionSize:=0]
      }
    }
    positionFullyClosedDate <- this.dat[CurrentPositionSize >= 0, first(date)]
    if(length(positionFullyClosedDate) != 0){
      this.dat <- this.dat[date <= positionFullyClosedDate]
    }
  }
  
  exit <- this.dat[!is.na(ExitAmount) | !is.na(LimitEntryTime)]
  return(exit)
}
# If current postion size is still open, add another entry
system.time(
  bt <- backtest(ask, bid, 
                 entry_fun=entry_function,
                 exit_fun=exit_fun,
                 CloseTradeOnOppositeSignal=TRUE, 
                 AddToPosition=FALSE,
                 TradeTimeLimit=years(2))
)

plot_returns(bt)

plot_strategy(bt, order_id=1, candle_period_mins=1)

