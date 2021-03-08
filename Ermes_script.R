
rm(list=ls()) 
library(tidyverse)
library(lubridate)
library(Rcpp)
library(xts)
library(TTR)
library(tidyquant)
library(caTools) #lowest/highest --> runmin/runmax
library(tibbletime)
library(gridExtra)
library(tidyr)


source("Ermes_source.R")


#########################
#in this file (script) we template all the necessary variables that a user should
#collect beforehand using our backtesting engine
#the file source, which is source above, contains the main function "ermes" 
#¸--> there is an outstanding error at line 247 file source
#which we call below at line 85 when all the inputs are calculated)
#the file source also contains a quick wrapper around the function read.csv 
#(not important, function ermes is the focus)
########################

#parse data
db <- parse(file_name = "es.txt")  #simple wrapper function around read.csv
sl_db <- parse(file_name = "es30.txt") 

#add technical indicators on condition database
db_xts <- as.xts(db[,2:5], order.by = db$datetime)
dn <- TTR::BBands(db_xts[,1:3])$dn %>% as_tibble()
up <- TTR::BBands(db_xts[,1:3])$up  %>% as_tibble() 
rsi <- TTR::RSI(db_xts[,3])  %>% as_tibble()
atr <- TTR::ATR(db_xts[,1:3])$atr  %>% as_tibble()
db <- db %>% 
    add_column(dn, up, rsi, atr) %>% 
    drop_na()

#add technical indicators on stop loss database
sl_db_xts <- as.xts(sl_db[,2:5], order.by = sl_db$datetime)
rsi <- TTR::RSI(sl_db_xts[,3]) %>% as_tibble()
atr <- TTR::ATR(sl_db_xts[,1:3])$atr %>% as_tibble()
stop_loss_db <- sl_db %>% 
    add_column(rsi, atr) %>% 
    drop_na()

#create outside parameters
db <- db %>% 
    mutate(condition_long = ifelse(low < dn, T, F)) %>% #long condition (if NULL --> = 0) 
    mutate(condition_short = ifelse(high > up, T, F)) %>% #short condition (if NULL --> = 0)
    mutate(ncon = ifelse(atr > mean(atr), 1, 2)) %>% #number of contracts
    mutate(limit_ref_entry = caTools::runmin(close, 3)) %>% #limit reference price for entry
    mutate(stop_ref_entry = caTools::runmax(close, 3)) %>% #stop reference price for entry
    mutate(stop_loss = 100) %>% 
    mutate(target = 100) %>% 
    drop_na()


cover_cond <- function(data){
    if (data$close - data$open > data$atr + 12){x <- T}else{x <- F}
    return(x)}
exit_long_cond <- function(data){
    if (data$rsi > 70){x <- T}else{x <- F}
    return(x)}

exit_short_cond <- function(data){
    if (data$rsi < 30){x <- T}else{x <- F}
    return(x)}

limit_ref_exit <- function(data){
    x <- data$close
    return(x)}

stop_ref_exit <- function(data){
    x <- data %>% caTools::runmax(close, 2)
    return(x)}

ncon <- function(data){
    if (data$atr > mean(data$atr)){x<-1}else{x<-2}
    return(x)}

res <- ermes(data = db, stop_loss_db = stop_loss_db,
                  entry_type = "market", 
                  exit_type = "market"
             );res %>% filter(touch != 0)

theme_set(theme_bw())
ggplot(data = res) + 
    geom_line(aes(x = Date, y = cumsum(pnl)), color = "blue") +
    geom_point(aes(x = Date, y = ifelse(pnl != 0, cumsum(pnl),0),color=factor(Color)))+
    scale_color_manual('Value',values=c('transparent','red'))+
    scale_x_date(date_labels="%m/%y",date_breaks  ="4 month")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    labs(title="Equity line", 
         y="Cumulated equity")+
    theme(legend.position = "none")
