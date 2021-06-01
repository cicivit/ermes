
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
    exit[, ExitAmount:=OrderSize*-1]
    exit[, CurrentPositionSize:=OrderSize+ExitAmount]
    exit[, ExitReason:='TakeProfit']
    exit[, OrigOrder:=Order_ID]
  } else if(sl_date < tp_date) {
    exit <- dis[date==sl_date]
    exit[, ExitTime:=sl_date]
    exit[, ExitPrice:=StopLoss]
    exit[, ExitAmount:=OrderSize*-1]
    exit[, CurrentPositionSize:=OrderSize+ExitAmount]
    exit[, ExitReason:='StopLoss']
    exit[, OrigOrder:=Order_ID]
  } else if(tp_date == farfuture & sl_date == farfuture) {
    exit <- dis[which.max(date), ]
    exit[, ExitTime:=date]
    exit[Side==1, ExitPrice:=last(Close.a)]
    exit[Side == -1, ExitPrice:=last(Close.b)] 
    exit[, ExitAmount:=OrderSize*-1]
    exit[, CurrentPositionSize:=OrderSize+ExitAmount]
    exit[, ExitReason:='StopLoss/TakeProfit never hit']
    exit[, OrigOrder:=Order_ID]
  } else if(tp_date == sl_date) {
    exit <- dis[date==tp_date]
    exit[, ExitTime:=tp_date]
    exit[, ExitPrice:=NA]
    exit[, ExitAmount:=OrderSize*-1]
    exit[, CurrentPositionSize:=OrderSize+ExitAmount]
    exit[, ExitReason:='Exit Price Unknown. TP/SL hit on same candle']
    exit[, OrigOrder:=Order_ID]
  }
  
  return(exit)
}

