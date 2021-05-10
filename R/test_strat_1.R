options(stringsAsFactors=FALSE)
library(quantmod)
library(data.table)
library(myFinanceTools)
ask <- readRDS('data/EUR_USD_M1_Ask.rds')
bid <- readRDS('data/EUR_USD_M1_Bid.rds')
ask <- ask['2009::2019']
bid <- bid['2009::2019']

source('ermes2.R')

dat <- mergeAskBid(ask, bid)

entry <- function(dat) {
  askcols <- str_detect(names(dat), '.a$')
  h1 <- to.hourly(xts(dat[, askcols, with=FALSE], order.by=dat$date), names=NULL)
  bbands_h1 <- BBands(HLC(h1))
  rsi_h1 <- RSI(Cl(h1))
  atr_h1 <- ATR(HLC(h1))
  
  m30 <- to.minutes30(xts(dat[, askcols, with=FALSE], order.by=dat$date), names=NULL)
  rsi_m30 <- RSI(Cl(m30))
  atr_m30 <- ATR(HLC(m30))
  
  h1$isBuy <- Lo(h1) < bbands$dn
  h1$isSell <- Hi(h1) > bbands$up
  
  
}


