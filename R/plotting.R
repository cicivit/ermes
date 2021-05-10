

plot_returns <- function(bt){
  setorder(bt, date)
  plot_ly(bt) %>%
    add_trace(x=~date, y=~cumsum(Returns), type='scatter', mode='lines+markers', color=I('black'))
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
