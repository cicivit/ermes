entry_on_MAcross <- function(dat, fast_n=60*5, slow_n=60*24*50, fastMA_fun=TTR::EMA, slowMA_fun=TTR::SMA) {
  # Input Signals
  dat[, EMAfast:=fastMA_fun(Close.a, fast_n)]
  dat[, SMAslow:=slowMA_fun(Close.a, slow_n)]
  dat[, lagFast:=shift(EMAfast, 1)]
  dat[, lagSlow:=shift(SMAslow, 1)]
  ask_cols <- names(dat)[str_detect(names(dat), '.a$')]
  ask <- dt_to_xts(dat[, c('date', ask_cols), with=FALSE])
  atr <- mergeHighLowTimes(ask, lgTP='h1', smTP='m1',
                           cols=c('High', 'Low', 'Close'),
                           fun=ATR, n=70)
  atr <- data.table(date=index(atr), atr)
  dat <- merge(dat, atr[, .(date, tr, atr)], by='date', all.x=TRUE)
  dat[, atr:=na.locf(atr, na.rm=FALSE)]
  dat[, tr:=na.locf(tr, na.rm=FALSE)]
  # isBuy/isSell
  dat[EMAfast > SMAslow & lagFast < lagSlow, OrderSize:=10000]
  dat[EMAfast < SMAslow & lagFast > lagSlow, OrderSize:= -10000]

  # TP/SL Buy orders
  dat[OrderSize>0, TakeProfit:=Close.a+tr*2]
  dat[OrderSize>0, StopLoss:=Close.a-(tr/2)]
  
  # TP/SL Sell orders
  dat[OrderSize<0, TakeProfit:=Close.b-tr*2]
  dat[OrderSize<0, StopLoss:=Close.b+(tr/2)]
  return(dat)
}