
mergeAskBid <- function(ask, bid) {
  if(is.xts(ask)) {
    ask <- data.table(date=index(ask), ask)
  }
  if(is.xts(bid)) {
    bid <- data.table(date=index(bid), bid)
  }
  
  dat <- merge(ask, bid, by='date', suffixes=c('.a', '.b'))
  return(dat)
}

xts_to_dt <- function(ts) {
  out <- data.table(date=as.POSIXct(index(ts)),
                    data.table(ts))
  return(out)
}


dt_to_xts <- function(dt) {
  out <- xts(OHLC(dt), order.by=as.POSIXct(dt$date))
  return(out)
}

getTimeFrameMins <- function(dat) {
  as.numeric(median(diff(tail(index(dat), 100))), units='mins')
}

mergeHighLowTimes <- function(ask, lgTP='h1', smTP='m1', cols='Close',
                              fun, ...) {
  lgTP <- tolower(lgTP)
  smTP <- tolower(smTP)
  chr1 <- substring(lgTP, 1,1)
  chr2 <- substring(lgTP, 2,3)
  if(nchar(chr2)==0) chr2 <- 1
  if(chr1=='m'){
    datLg <- to.minutes(ask, k=chr2, indexAt='endof', name=NULL)
  } else if(chr1=='h') {
    datLg <- to.hourly(ask, k=chr2, indexAt='endof', name=NULL)
  } else if(chr1=='d') {
    datLg <- to.daily(ask, k=chr2, indexAt='endof', name=NULL, drop.time=FALSE)
  } else if(chr1=='w'){
    datLg <- to.weekly(ask, k=chr2, indexAt='endof', name=NULL, drop.time=FALSE)
  }
  
  chr1 <- substring(smTP, 1,1)
  chr2 <- substring(smTP, 2,3)
  if(nchar(chr2)==0) chr2 <- 1
  if(chr1=='m'){
    datSm <- to.minutes(ask, k=chr2, indexAt='startof', name=NULL)
  } else if(chr1=='h') {
    datSm <- to.hourly(ask, k=chr2, indexAt='startof', name=NULL)
  } else if(chr1=='d') {
    datSm <- to.daily(ask, k=chr2, indexAt='startof', name=NULL, drop.time=FALSE)
  } else if(chr1=='w'){
    datSm <- to.weekly(ask, k=chr2, indexAt='startof', name=NULL, drop.time=FALSE)
  }
  
  out <- fun(datLg[, cols], ...)
  out2 <- merge(datSm, out, all=TRUE)
  out3 <- na.locf(out2, fromLast=FALSE, na.rm=FALSE)
  return(out3)
}
