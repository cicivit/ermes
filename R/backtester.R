
#' Backtest a financial intrument
#' 
#' Write details section here....
#' 
#' @param ask as an xts object of OHLC ask prices
#' @param bid as an xts object of OHLC ask prices
#' @param entry_fun a function that takes in bid/ask prices and returns a data.frame/data.table with mandatory columns: date, OrderSize, and optional columns: OrderType = c('Market', 'Limit', 'Stop'), StopLoss, TakeProfit
#' @param entry_args named list of arguments that accompany the entry function
#' @param exit_fun a function that takes in bid/ask prices + the output of the entry function and returns...
#' @param resize_fun a function that tells the backtester when it's appropriate to resize a current order
#' @param CloseTradeOnOppositeSignal should the backtester close the current trade when an entry order in the opposite direction appears
#' @param TradeTimeLimit having a time limit increases performance
#' @return a data.table object with...
#' @examples
backtest <- function(ask, bid, 
                     entry_fun, entry_args=list(),
                     exit_fun=NULL, exit_args=list(),
                     CloseTradeOnOppositeSignal=TRUE,
                     AddToPosition=FALSE,
                     TradeTimeLimit=lubridate::weeks(4)) {
  
  # Load bid/ask historical data
  dat <- mergeAskBid(ask, bid)
  
  # Create entry orders, takes in a list of arguments for the function (entry_args)
  arg_names <- names(formals(entry_fun))
  args <- c(list(copy(dat)), entry_args)
  names(args)[1] <- arg_names[1]
  dat <- rlang::exec('entry_fun', !!!args)
  
  entry <- dat[OrderSize!=0 & !is.na(OrderSize)]
  # Return columns for prev/next order directions/sizes
  entry[, prev_order:=shift(OrderSize)]
  entry[, next_order:=shift(OrderSize, type='lead')]
  
  # Estimate the timeframe of our backtesting data in minutes
  mins <- getTimeFrameMins(ask)
  
  # Entry time will be the close of the current candle
  entry[, EntryTime:=date + lubridate::minutes(mins)]
  entry[, Side:=sign(OrderSize)]
  
  # If EntryPrice isn't supplied by the entry_fun, create it
  if(!'EntryPrice' %in% names(entry)) {
    entry[, EntryPrice:=ifelse(Side>0, Close.a, Close.b)]
  }
  
  # If no entry type, all is 'Market'
  if(!'EntryType' %in% names(entry)) {
    entry[, EntryType:='Market']
  }
  
  entry[, Order_ID:=1:nrow(entry)]
  # Entry loop ------------------
  # Loop through all the market entry points and get result for each trade
  results <- data.table()
  prevEndDate <- first(entry)$date
  for(iter in 1:nrow(entry)) {
    
    this.entry <- entry[iter,]
    if(AddToPosition==FALSE) {
      if(this.entry$date < prevEndDate) {
        #print('Skipping Order', Trade Open)
        next
      } 
    }
    
    if(CloseTradeOnOppositeSignal) {
      nextOppSignal <- first(entry[date > this.entry$date & 
                                    sign(OrderSize) != sign(this.entry$OrderSize), EntryTime])
      this.dat <- dat[date>=this.entry$date & date < nextOppSignal]
    } else {
      this.dat <- dat[date>=this.entry$date & date < (this.entry$date + TradeTimeLimit)]
    }
    this.dat[, Side:=first(this.entry$Side)]
    
    this.dat[, OrderSize:=this.entry$OrderSize]
    this.dat[, EntryPrice:=this.entry$EntryPrice]
    this.dat[, Order_ID:=this.entry$Order_ID]
    
    # If TP / SL is present
    if('TakeProfit' %in% names(this.dat)) {
      this.dat[, TakeProfit:=this.entry$TakeProfit]
    } else {
      this.dat[, TakeProfit:=Inf * this.entry$Side]
    }
    if('StopLoss' %in% names(this.dat)) {
      this.dat[, StopLoss:=this.entry$StopLoss]
    } else {
      this.dat[, StopLoss:= -Inf * this.entry$Side]
    }
    
    # Limit/Stop Orders -----------------
    # If the entry type is limit or stop, cut out the data before that price is hit
    if(this.entry$EntryType=='Limit') {
      if(this.entry$Side == 1) {
        trade_entry_time <- first(this.dat[ Low.a <= this.entry$EntryPrice])$date
      } else if(this.entry$Side == -1) {
        trade_entry_time <- first(this.dat[ High.b >= this.entry$EntryPrice])$date
      }
      if(length(trade_entry_time) == 0) {
        next
      }
      this.dat <- this.dat[date >= trade_entry_time]
    }
    
    if(this.entry$EntryType=='Stop') {
      if(this.entry$Side == 1) {
        trade_entry_time <- first(this.dat[ High.a >= this.entry$EntryPrice])$date
      } else if(this.entry$Side == -1) {
        trade_entry_time <- first(this.dat[ Low.b <= this.entry$EntryPrice])$date
      }
      if(length(trade_entry_time) == 0) {
        next
      }
      this.dat <- this.dat[date >= trade_entry_time]
    }
    
    
    if(nrow(this.dat) <= 2){
      warning(paste0('historical data is not granular enough: ', this.entry$date))
      next
    }
    
    # Trade Exit -------------------
    if(!is.null(exit_fun)) {
      arg_names <- names(formals(exit_fun))
      args <- c(list(copy(this.dat)), exit_args)
      names(args)[1] <- arg_names[1]
      exit <- rlang::exec('exit_fun', !!!args)
    } else {
      # If no exit function just use the TP/SL values
      # and if they're not there, just the last date
      exit <- exitTP_SL(this.dat)
    }
    prevEndDate <- last(exit)$ExitTime
    # Duplicate our entry info for every exit line (for partial closes)
    for(exit_rows in 1:nrow(exit)){
      if(exit_rows == 1) next
      this.entry <- rbind(this.entry, this.entry)
    }
    thistrade <- cbind(this.entry, exit[, !names(exit) %in% names(this.entry), with=FALSE])
    thistrade[, Returns := Side * (ExitPrice - EntryPrice) * ExitAmount]
    results <- rbind(results, thistrade)
    print(paste0('Trade ', thistrade$Order_ID, ': ', round(thistrade$Returns, 1), ' | Total: ', 
                 last(cumsum(round(results$Returns, 1)))))
  }
 
  return(results)
}