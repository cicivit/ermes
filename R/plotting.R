

plot_returns <- function(bt){
  pdat <- bt$results[!is.na(Returns)]
  setorder(pdat, date)
  plot_ly(pdat) %>%
    add_trace(x=~date, y=~cumsum(Returns), text=~paste('Order ID:', Order_ID), type='scatter', mode='lines+markers', color=I('black'))
}

plot_trades <- function(bt, ask, order_ids=1) {
  normalCols <- c('date', 'Open.a', 'High.a', 'Low.a', 'Close.a', 'Volume.a', 'Open.b', 'High.b', 'Low.b', 'Close.b',
                  'Volume.b', 'OrderSize', 'TakeProfit', 'StopLoss', 'prev_order', 'next_order',
                  'Entry_Time', 'Side', 'Entry_Price', 'Entry_Type', 'Order_ID', 'Exit_Price', 'Exit_time', 'Exit_Amount',
                  'Exit_Reason', 'Orig_Order', 'Returns')
  userCols <- names(bt)[!names(bt) %in% normalCols]
  
  setorder(bt, date)
  bt <- bt[Order_ID %in% order_ids]
  if(is.xts(ask)) {
    ts <- ask[paste0(bt$date, '/', bt$Exit_time)]
    ts <- xts_to_dt(ts)
  }
  names(ts) <- tolower(names(ts))

  plot_ly(ts) %>%
    add_trace(x=~date, type="candlestick",
              open = ~open, close = ~close,
              high = ~high, low = ~low)
}

plot_strategy <- function(bt, order_id=1, candle_period_mins=30) {
  
  pdat <- bt$data
  normalCols <- c('date', 'Open.a', 'High.a', 'Low.a', 'Close.a', 'Volume.a', 'Open.b', 'High.b', 'Low.b', 'Close.b',
                  'Volume.b', 'OrderSize', 'TakeProfit', 'StopLoss', 'prev_order', 'next_order',
                  'Entry_Time', 'Side', 'Entry_Price', 'Entry_Type', 'Order_ID', 'Exit_Price', 'Exit_time', 'Exit_Amount',
                  'Exit_Reason', 'Orig_Order', 'Returns', 'EntryType', 'EntryPrice', 'EntryTime', 'ExitTime',
                  'ExitPrice', 'ExitAmount', 'ExitReason', 'OrigOrder', 'LimitOrderID')
  userCols <- names(pdat)[!names(pdat) %in% normalCols]
  
  
  
  start <- bt$data[Order_ID==order_id, first(EntryTime)] - hours(5)
  end <- bt$data[Order_ID==order_id, last(ExitTime)] + hours(24)
  pdat <- pdat[date >= start & date <= end]
  
  candles <- to.minutes(dt_to_xts(pdat[, .(date, Open.a, High.a, Low.a, Close.a)]), 
                        k=candle_period_mins, indexAt='startof', name=NULL)
  candles <- xts_to_dt(candles)
  candles <- candles[date>=start & date <=end]
  side <- first(pdat[Order_ID==order_id, Side])
  if(side > 0) {
    entrySymbol <- 'triangle-ne-dot'
    exitSymbol <- 'triangle-sw-dot'
  } else if(side < 0) {
    entrySymbol <- 'triangle-se-dot'
    exitSymbol <- 'triangle-nw-dot'
  }
  p <- plot_ly(pdat[Order_ID==order_id]) %>%
    add_trace(x=candles$date, type="candlestick", name='Candles',
              open = candles$Open, close = candles$Close,
              high = candles$High, low = candles$Low) %>%
    add_trace(x=~EntryTime, y=~EntryPrice, text=~paste('Position:', OrderSize), 
              type='scatter', mode='markers', name='Entry',
              marker=list(symbol=entrySymbol, size=15,
                          line = list(
                            color = I("black"),
                            width = 2))) %>%
    add_trace(x=~ExitTime, y=~ExitPrice, type='scatter', text=~paste('Position:', CurrentPositionSize, 
                                                                     'Exit Reason:', ExitReason),
              mode='markers', name='Exit',
              marker=list(symbol=exitSymbol, size=15,
                          line = list(
                            color = I("black"),
                            width = 2))) %>%
    layout(xaxis = list(rangeslider = list(visible = FALSE)),
           hovermode='x unified') 
  if('LimitEntryTime' %in% names(pdat)) {
    p <- p %>% add_trace(x=~LimitEntryTime, y=~LimitOrderPrice, text=~paste('Position: ', CurrentPositionSize), 
              type='scatter', mode='markers', name='Addition',
              marker=list(symbol='cross-dot',line = list(color = I("black"), width = 2)))
  }
  
  for(col in userCols) {

    yyy <- pdat[ , col, with=FALSE][[1]]
    if(class(yyy) == 'numeric' & sum(!is.na(yyy)) != 0){

      mean <- mean(pdat$Close.a, na.rm=TRUE)
      meanCol <- mean(yyy, na.rm=TRUE)
      if(is.na(meanCol)) {
        next
      }
      upBound <- mean + (mean * .3)
      dnBound <- mean - (mean * .3)
      
      if(meanCol > upBound | meanCol < dnBound) {
        if(exists('p2')) {
          p2 <- p2 %>%
            add_trace(x=pdat$date, y=yyy, type='scatter', mode='lines', name=col) %>%
            layout(hovermode='x unified')
        } else {
          p2 <- plot_ly(pdat) %>%
            add_trace(x=~date, y=~get(col), type='scatter', mode='lines', name=col)
        }  
      } else {
        p <- p %>%
          add_trace(x=pdat$date, y=yyy, type='scatter', mode='lines', name=col)
      }
    }
  }
  
  if(exists('p2')) {
    return(subplot(p, p2, nrows=2, shareX=TRUE)%>%
             config(displayModeBar=FALSE))
  } else {
    return(p)
  }
}