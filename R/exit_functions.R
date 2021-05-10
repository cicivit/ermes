# Function returns Exit, Exit_Price, and Exit_Reason
exitTP_SL_time <- function(dis, time_hours) {

  dis <- dis[date <= date + hours(time_hours)]
  side <- first(dis)$Side
  if(side==1) {
    tp_date <- first(dis[High.b >= TakeProfit])$date
    sl_date <- first(dis[Low.a <= StopLoss])$date
  } else if(side== -1) {
    tp_date <- first(dis[Low.a <= TakeProfit])$date
    sl_date <- first(dis[High.b >= StopLoss])$date
  }
  
  # Make sure there's a value for tp/sl date even if it's not in the dataset
  farfuture <- as.POSIXct('3025-01-01')
  tp_date <- min(tp_date, farfuture)
  sl_date <- min(sl_date, farfuture)
  
  
  if(tp_date < sl_date) {
    exit <- dis[date==tp_date]
    exit[, ExitTime:=tp_date]
    exit[, ExitPrice:=TakeProfit]
    exit[, ExitAmount:=OrderSize]
    exit[, ExitReason:='TakeProfit']
    exit[, OrigOrder:=Order_ID]
  } else if(sl_date < tp_date) {
    exit <- dis[date==sl_date]
    exit[, ExitTime:=sl_date]
    exit[, ExitPrice:=StopLoss]
    exit[, ExitAmount:=OrderSize]
    exit[, ExitReason:='StopLoss']
    exit[, OrigOrder:=Order_ID]
  } else if(tp_date == farfuture & sl_date == farfuture) {
    exit <- dis[which.max(date), ]
    exit[, ExitTime:=date]
    exit[Side==1, ExitPrice:=last(Close.a)]
    exit[Side == -1, ExitPrice:=last(Close.b)] 
    exit[, ExitAmount:=0]
    exit[, ExitReason:=ifelse(ExitTime==(date + hours(time_hours)), 
                               'Out of time', 'StopLoss/TakeProfit never hit')]
    exit[, Orig_Order:=Order_ID]
  } else if(tp_date == sl_date) {
    exit <- dis[date==tp_date]
    exit[, ExitTime:=tp_date]
    exit[, ExitPrice:=NA]
    exit[, ExitAmount:=OrderSize]
    exit[, ExitReason:='Exit Price Unknown. TP/SL hit on same candle']
    exit[, OrigOrder:=Order_ID]
  }
  
  exit <- dis[which.max(date), ]
  exit[, ExitTime:=date]
  exit[Side==1, ExitPrice:=last(Open.a)]
  exit[Side == -1, ExitPrice:=last(Open.a)] 
  exit[, ExitAmount:=OrderSize]
  exit[, ExitReason:='Out of Time']
  exit[, OrigOrder:=exit$Order_ID]
  return(exit)
}

exitTP_SL <- function(dis) {
  side <- first(dis)$Side

  if(side==1) {
    tp_date <- first(dis[High.b >= TakeProfit])$date
    sl_date <- first(dis[Low.a <= StopLoss])$date
  } else if(side== -1) {
    tp_date <- first(dis[Low.a <= TakeProfit])$date
    sl_date <- first(dis[High.b >= StopLoss])$date
  }
  
  # Make sure there's a value for tp/sl date even if it's not in the dataset
  farfuture <- as.POSIXct('3025-01-01')
  tp_date <- min(tp_date, farfuture)
  sl_date <- min(sl_date, farfuture)
  
  
  if(tp_date < sl_date) {
    exit <- dis[date==tp_date]
    exit[, ExitTime:=tp_date]
    exit[, ExitPrice:=TakeProfit]
    exit[, ExitAmount:=OrderSize]
    exit[, ExitReason:='TakeProfit']
    exit[, OrigOrder:=Order_ID]
  } else if(sl_date < tp_date) {
    exit <- dis[date==sl_date]
    exit[, ExitTime:=sl_date]
    exit[, ExitPrice:=StopLoss]
    exit[, ExitAmount:=OrderSize]
    exit[, ExitReason:='StopLoss']
    exit[, OrigOrder:=Order_ID]
  } else if(tp_date == farfuture & sl_date == farfuture) {
    exit <- dis[which.max(date), ]
    exit[, ExitTime:=date]
    exit[Side==1, ExitPrice:=last(Close.a)]
    exit[Side == -1, ExitPrice:=last(Close.b)] 
    exit[, ExitAmount:=0]
    exit[, ExitReason:='StopLoss/TakeProfit never hit']
    exit[, OrigOrder:=Order_ID]
  } else if(tp_date == sl_date) {
    exit <- dis[date==tp_date]
    exit[, ExitTime:=tp_date]
    exit[, ExitPrice:=NA]
    exit[, ExitAmount:=OrderSize]
    exit[, ExitReason:='Exit Price Unknown. TP/SL hit on same candle']
    exit[, OrigOrder:=Order_ID]
  }
  
  return(exit)
}

