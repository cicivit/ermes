
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
library(ggplot2)


source("Ermes_source.R")


#########################
#in this file (script) we template all the necessary variables that a user should
#collect beforehand using our backtesting engine
#the file source, which is source above, contains the main function "ermes"
#?--> there is an outstanding error at line 247 file source
#which we call below at line 85 when all the inputs are calculated)
#the file source also contains a quick wrapper around the function read.csv
#(not important, function ermes is the focus)
########################

#parse data
db <- parse(file_name = "es.txt")  #simple wrapper function around read.csv
sl_db <- parse(file_name = "es30.txt")

#add technical indicators on condition database
################################################################################################
# 1. Dangerous to use column numbers instead of names. Quantmod has wrapper functions for OHLC data
# HLC(db_xts) OpHi(db_xts) Cl(db_xts), etc. (see ?OHLC.Transformations)
# 2. The ermes function should not be reliant on the input data having indicator columns
#   These should be calculated in the exit_short_cond, etc. functions. Then these functions
#   should be required by and passed to the ermes function
################################################################################################
db_xts <- as.xts(OHLC(db), order.by = db$datetime)
dn <- TTR::BBands(HLC(db_xts))$dn %>% as_tibble()
up <- TTR::BBands(HLC(db_xts))$up  %>% as_tibble()
rsi <- TTR::RSI(Cl(db_xts))  %>% as_tibble()
atr <- TTR::ATR(HLC(db_xts))$atr  %>% as_tibble()
db <- db %>%
    add_column(dn, up, rsi, atr) %>%
    drop_na()

#add technical indicators on stop loss database
sl_db_xts <- as.xts(OHLC(sl_db), order.by = sl_db$datetime)
rsi <- TTR::RSI(Cl(sl_db_xts)) %>% as_tibble()
atr <- TTR::ATR(HLC(sl_db_xts))$atr %>% as_tibble()
stop_loss_db <- sl_db %>%
    add_column(rsi, atr) %>%
    drop_na()
################################################################################################
# runmin/runmax looks into the future, use the option 'align' to make sure it stays in the past
# ncon should be caluculated from ATR previous to that time point, not all ATR values --
# ATR changes significantly over time plot(TTR::ATR(db_xts[,1:3])$atr)
# mean ATR over the full period is probably not what you want, and gives future information to
# past results
################################################################################################
#create outside parameters
db <- db %>%
    mutate(condition_long = ifelse(low < dn, T, F)) %>% #long condition (if NULL --> = 0)
    mutate(condition_short = ifelse(high > up, T, F)) %>% #short condition (if NULL --> = 0)
    mutate(ncon = ifelse(atr > mean(atr), 1, 2)) %>% #number of contracts
    mutate(limit_ref_entry = caTools::runmin(close, 3, align='right')) %>% #limit reference price for entry
    mutate(stop_ref_entry = caTools::runmax(close, 3, align='right')) %>% #stop reference price for entry
    mutate(stop_loss = 100) %>%
    mutate(target = 100) %>%
    drop_na()

##############################################################################################
# Since these functions will change for various strategies, they need to be required by
# your ermes function
# Also, all these functions rely on data having rsi, atr, runmax calculated. This should
# occur within these functions to keep them modular
##############################################################################################
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
## Need to use align on the runmax function. Otherwise you are telling your backtest the future
stop_ref_exit <- function(data){
    x <- data %>% caTools::runmax(close, 2)
    return(x)}

ncon <- function(data){
    if (data$atr > mean(data$atr)){x<-1}else{x<-2}
    return(x)}

res <- ermes(data = db, stop_loss_db = stop_loss_db,
                  entry_type = "market",
                  exit_type = "market"
             )
res %>% filter(touch != 0)

theme_set(theme_bw())
#############################################################################################
# pnl column does not exist, neither does the Color object
#############################################################################################
ggplot(data = res) +
    geom_line(aes(x = Date, y = cumsum(running_pnl)), color = "blue") +
    #geom_point(aes(x = Date, y = ifelse(running_pnl != 0, cumsum(running_pnl),0)))+
    scale_color_manual('Value',values=c('transparent','red'))+
    scale_x_date(date_labels="%m/%y",date_breaks  ="4 month")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    labs(title="Equity line",
         y="Cumulated equity")+
    theme(legend.position = "none")
