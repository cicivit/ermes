if(side > 0) {
  entrySymbol <- 'triangle-ne-dot'
  exitSymbol <- 'triangle-sw-dot'
} else if(side < 0) {
  entrySymbol <- 'triangle-se-dot'
  exitSymbol <- 'triangle-nw-dot'
}  


p1 <- plot_ly(this.dat) %>%
  add_trace(x=~date, type="candlestick", name='Candles',
            open = ~Open.a, close = ~Close.a,
            high = ~High.a, low = ~Low.a) %>%
  add_trace(x=~EntryTime, y=~EntryPrice, text=~paste('Position: ', CurrentPositionSize), 
            type='scatter', mode='markers', name='Entry',
            marker=list(symbol=entrySymbol, size=15,
                        line = list(
                          color = I("black"),
                          width = 2))) %>%
  add_trace(x=~ExitTime, y=~ExitPrice, text=~paste('Position: ', CurrentPositionSize), 
            type='scatter', mode='markers', name='Exit',
            marker=list(symbol=exitSymbol, size=15,
                        line = list(
                          color = I("black"),
                          width = 2))) %>%
  add_trace(x=~LimitEntryTime, y=~LimitOrderPrice, text=~paste('Position: ', CurrentPositionSize), 
            type='scatter', mode='markers', name='Addition',
            marker=list(symbol='cross-dot',line = list(color = I("black"), width = 2))) %>%
  layout(xaxis = list(rangeslider = list(visible = FALSE)),
         hovermode='x unified') 
p2 <- plot_ly(this.dat) %>%
  add_trace(x=~date, y=~rsi10h, type='scatter', mode='lines', name='RSI 10h') %>%
  layout(hovermode='x unified')
subplot(p1, p2, nrows=2, shareX = TRUE)

plot(this.dat$CurrentPositionSize, type='l')
