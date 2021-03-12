parse <- function(file_name = "es.d60.txt"){
    db <- read.table(file_name, sep = ",", header = TRUE) %>% as_tibble() %>%
        dplyr::select(Date, High, Low, Close, Open, Time) %>%
        rename(high = High, low = Low, close = Close, open = Open)%>%
        mutate(Date = mdy(Date)) %>%
        mutate(datetime = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M")) %>%
        arrange(Date, datetime)
    return(db)
}

ermes <- function(data, stop_loss_db, entry_type = c("market", "limit", "stop"),
                              exit_type = c("market", "limit", "stop")){
    out <- data.frame()
    #initialize variables for evaluation
    data <- data %>%
        mutate(pos = 0) %>%
        mutate(mktPos = 0) %>%
        mutate(c_long = 0) %>%
        mutate(c_short = 0) %>%
        mutate(long_short = 0) %>%
        mutate(c_final = 0) %>%
        mutate(e_final = 0) %>%
        mutate(cover_cond = 0) %>%
        mutate(c_limit = 0) %>%
        mutate(e_limit = 0)

    data <- as.data.frame(data)

    #handler for NULL parameters
    if (is.null(data$condition_long) == T){data$condition_long = 0}
    if (is.null(data$condition_short) == T){data$condition_short = 0}
    if (is.null(data$limit_ref_entry) == T){data$limit_ref_entry = 0}
    if (is.null(data$stop_ref_entry) == T){data$stop_ref_entry = 0}

    ################################################################################
    # Is j always go to start at 3 for all strategies, the intial j needs to
    # be calculated somehow
    ################################################################################
    j <- 3
    while (j <= nrow(data)){
        ###############################################################################
        # Possibly large speedup: instead of accessing data$variable[j] repeatedly,
        # create a 1-row dataframe like: thisrow <- data[j, ] and do calculations accessing
        # this row instead of the whole data.frame
        # Or better use objects in xts or data.table format instead of base r data.frames
        ###############################################################################
        
        if((j%%1000) == 0){print(j)} #print counter each 1000th iteration

        #evaluate condition_long only if mktPos is == 0 (i.e. not in position)
        #if the trade is already placed (i.e. mktPos is !=0), then do not evaluate condition
        if (data$condition_long[j] == T & data$mktPos[j-1] == 0) {
            data$c_long[j] <- 1
        } else {
            data$c_long[j] <- 0
        }

        #evaluate condition_short only if mktPos is == 0 (i.e. not in position)
        #if the trade is already placed (i.e. mktPos is !=0), then do not evaluate condition
        if (data$condition_short[j] == T & data$mktPos[j-1] == 0) {
            data$c_short[j] <- -1
        } else {
            data$c_short[j] <- 0
        }

        #calculate whether the condition (either long or short) is true
        #it works as a "+" as long as the conditions (both long ans short) are mutually exclusive
        data$long_short[j] <- data$c_long[j]+data$c_short[j]

        #evaluate the final condition when no trade is open
        if (data$long_short[j] != 0 & data$long_short[j-1] != 0){
            data$c_final[j] <- 0
        } else if (data$long_short[j] != 0) {
            data$c_final[j] <- data$long_short[j]
        } else {
            data$c_final[j] <- 0
        }

        #################################################################################
        # These if statements need ()
        ################################################################################
        if ((entry_type == "market" & data$c_final[j] == 0) | (entry_type != "market" & data$c_limit[j] == 0)){
            data$pos <- 0
            data$mktPos <- 0
            j <- j + 1
        } else if ((entry_type == "market" & data$c_final[j] != 0) | (entry_type != "market" & data$c_limit[j] != 0)){


            if (entry_type == "market"){
                ###############################################################################
                # Object db does not exist in this function, should be object: data
                ###############################################################################
                join_data <- data[j:nrow(data),c("datetime", "mktPos", "c_final",
                                               "stop_loss", "target")] %>% as_tibble()
                ############################################################################################
                # datetime in OHLC data usually refers to the Open time, so this data probably needs to be
                # datetime[j+1], or start <- join_data[2, 'datetime']
                ############################################################################################
                #start <- join_data %>% as_tibble() %>% filter(datetime == min(datetime)) %>%
                #    dplyr::select(datetime) # simplified: join_data[1, 'datetime']
                start <- join_data[1, 'datetime']
                df <- stop_loss_db %>%
                    filter(as.numeric(datetime) >= as.numeric(start)) %>% # no need to convert to numeric
                    mutate(touch = 0) %>%
                    left_join(., join_data, by = "datetime") %>%
                    rename(condition = c_final)


            } else {
                ###############################################################################
                # Object db does not exist in this function, should be object: data
                ###############################################################################
                join_data <- data[j:nrow(data),c("datetime", "mktPos", "c_limit",
                                               "stop_loss", "target")] %>% as_tibble()

                start <- join_data %>% as_tibble() %>% filter(datetime == min(datetime)) %>%
                    dplyr::select(datetime)

                df <- stop_loss_db %>%
                    filter(as.numeric(datetime) >= as.numeric(start)) %>%
                    mutate(touch = 0) %>%
                    left_join(., join_data, by = "datetime") %>%
                    rename(condition = c_limit)
            }

            #fill the NA values that are created from left_joining a larger dataset with a smaller one
            df <- df %>% tidyr::fill(mktPos, condition, stop_loss, target,  .direction = "up")
            
            #################################################################################################
            # There are NAs in stop_loss and target that create errors when df goes beyond data's timeframe
            # This is a temporary solution
            #################################################################################################
            df <- df[!is.na(df$stop_loss), ]
            #initialize variables into the while loop
            #########################################################################
            # Possible moderate speed-up is to create these columns outside of loop
            #########################################################################
            if (is.null(df$cover_cond) == T){ df$cover_cond<- 0}
            if (is.null(df$e_final) == T){  df$e_final<- 0}
            if (is.null(df$limit) == T){  df$limit<- 0}
            if (is.null(df$pos) == T){  df$pos<- 0}
            if (is.null(df$mktPos) == T){  df$mktPos<- 0}
            if (is.null(df$first_cash) == T){ df$first_cash <- 0}
            if (is.null(df$cf) == T){  df$cf<- 0}
            if (is.null(df$running_pnl) == T){ df$running_pnl <- 0}


            #this is a silly trick: if no target/stop_loss is provided, then put a huge value
            #to make sure that such treshold is never touched
            #######################################################################
            # Better to use Inf instead of huge number (but not critical)
            #######################################################################
            if (is.null(df$stop_loss) == T){df$stop_loss = 1e20}
            if (is.null(df$target) == T){df$target = 1e20}


            for (i in 3:nrow(df)) {

                #evaluate conver_condition if and only if mktPos != 0
                #you want to check whether to close a trade only when you have a trade in place

                #################################################################################
                # cover_cond relies on df already having ATR calculated,
                # better to not assume this. cover_cond() should calc atr
                #################################################################################
                if (df$mktPos[i-1] != 0 & cover_cond(df[i,]) == T){
                    df$cover_cond[i] <- 1
                } else {
                    df$cover_cond[i] <- 0
                }

                #check if you have to close a trade because the condition to cover is true
                #otherwise:
                #calculate whether the exit condition (either long or short) is true
                #it works as a "+" as long as the conditions (both exit long ans exit short) are mutually exclusive
                ####################################################################################
                # Do you really want to close a long position if exit_short_cond() is true?
                ####################################################################################
                if (df$cover_cond[i] != 0){
                    df$e_final[i] <- 1
                } else if(df$pos > 0) {
                    df$e_final[i] <- as.numeric(exit_long_cond(df[i,]))
                } else if(df$pos < 0) {
                    as.numeric(exit_short_cond(df[i,]))
                }

                #if exit_type is limit/stop, then you have to change the exit
                if (exit_type == "limit"){

                    if (df$e_final[i-1]!= 0 & (df$open[i-2]-df$open[i-1])>= abs(df$open[i-1]-limit_ref_exit(df[i-1,]))){
                        df$e_limit[i] <- df$e_final[i-1]
                    } else {
                        df$e_limit[i] <- 0
                    }

                } else if (exit_type == "stop"){

                    if (df$e_final[i-1]!= 0 & (df$open[i-1]-df$open[i-2])>= abs(df$open[i-1]-stop_ref_exit(df[i-1,]))){
                        df$e_limit[i] <- df$e_final[i-1]
                    } else {
                        df$e_limit[i] <- 0
                    }
                #####################################################################################
                # If you want to backtest a market exit, you will need tick level data
                #####################################################################################
                } else if (exit_type == "market"){

                    df <- df

                } else {
                    stop("entry_type must be either market, limit or stop")
                }

                #when the condition is true, then you trade and you are in position
                #you close the position when the period before you have an exit signal != 0
                #and if the order for exit is not market, then you have to use e_limit instead of e_final
                if (exit_type == "market"){
                    if (df$condition[i-2] != 0){
                        df$pos[i-1] <- df$condition[i-2]
                    } else if (df$pos[i-2] != 0 & df$e_final[i-2] == 0){
                        df$pos[i-1] <- df$pos[i-2]
                    } else {
                        df$pos[i-1] <- 0
                    }
                } else {
                    if (df$condition[i-2] != 0){
                        df$pos[i-1] <- df$condition[i-2]
                    } else if (df$pos[i-2] != 0 & df$e_limit[i-2] == 0){
                        df$pos[i-1] <- df$pos[i-2]
                    } else {
                        df$pos[i-1] <- 0
                    }
                }

                #update mktPos as the product of number of contract and position
                df$mktPos[i-1] <- ncon(df[i-1,])*df$pos[i-1]

                #if mktPos != 0 --> then you record the opening cash_flow
                #positive or negative depending on the sign of mktPos (inverse relationship)
                if (df$condition[i-2] != 0){
                    df$first_cash[i-1] <- -df$mktPos[i-1]*df$open[i-1]
                }

                #copies the value of first_cash in each row where there is a trade in position (i.e. position != 0)
                if (df$first_cash[i-1] != 0){
                    df$cf[i-1] <- df$first_cash[i-1]
                } else if (df$pos[i-1] != 0){
                    df$cf[i-1] <- df$cf[i-2]
                } else {
                    df$cf[i-1] <- 0
                }

                #calculates the pnl in each period (not cumulated, but live pnl)
                #: if position == 0 --> it is the sum of the first cash flow (cf) + the live market value of the position (open*mktPos)
                if (df$pos[i-1] != 0){
                    df$running_pnl[i-1] <- df$cf[i-1] + df$open[i-1]*df$mktPos[i-1]
                } else {
                    df$running_pnl[i-1] <- 0
                }

                #add thresholds of target and stop_loss
                #if these are touched, then record a profit/loss respectively equal to the same amount of column target/stop_loss
                if (df$running_pnl[i-1] < -df$stop_loss[i-1]*abs(df$mktPos[i-1])){
                    df$running_pnl[i-1] <- -df$stop_loss[i-1]*abs(df$mktPos[i-1])
                    df$touch[i-1] <- 1
                } else if (df$running_pnl[i-1] > df$target[i-1]*abs(df$mktPos[i-1])) {
                    df$running_pnl[i-1] <- df$target[i-1]*abs(df$mktPos[i-1])
                    df$touch[i-1] <- 1
                } else if (df$e_final[i-1] != 0){
                    df$touch[i-1] <- 1
                    df$running_pnl[i-1] <- df$running_pnl[i-1]
                } else {
                    df$running_pnl[i-1] <- df$running_pnl[i-1]
                }

                if (df$touch[i-1] != 0) break
            }

            #now we need to join back on the lower frequency database
            inverse_join_db <- df[1:i-1,]
            inverse_join_db <- as_tibble(inverse_join_db) %>%
                dplyr::select(datetime, touch, condition, pos, mktPos, running_pnl)

            x <- inverse_join_db %>%
                full_join(., data, by = "datetime")

            x <- x %>% tidyr::fill(names(x),.direction = "up")
            x <- x %>% mutate(pos = ifelse(is.na(pos.x) == TRUE, pos.y, pos.x)) %>%
                mutate(mktPos = ifelse(is.na(mktPos.x) == TRUE, mktPos.y, mktPos.x)) %>%
                dplyr::select(-pos.x, -pos.y, -mktPos.x, -mktPos.y)
            out <- rbind(out, inverse_join_db)
            #the external loop on the external database should start where the
            #loop on the higher frequency database stopped
            #for this reason, we have to set-up a new j, the iteration variable on the external loop,
            #such that the external loop starts from the latest checked date on the internal loop
            # HERE I DID NOT MANAGE TO FIX THE J LEVEL PROPERLY
            #this should be the last thing missing on this project
            # j <- row_number + where
            trade_exit_dt <- df[df$touch!=0, 'datetime'][[1]]
            if(is.null(trade_exit_dt) | length(trade_exit_dt) == 0) break
            if(trade_exit_dt >= max(data$datetime)) break
            
            next_j_dt <- first(data$datetime[data$datetime > trade_exit_dt])
            j <- which(data$datetime == next_j_dt)
        }
        
        # j <- first(data$datetime>currenttime)
        print(j)
        #########################################################################################
        # return() function needs to be outside of while loop
        #########################################################################################

    }
    return(out)
}



