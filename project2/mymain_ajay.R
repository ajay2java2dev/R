###############################################################################
library(forecast)
library(lubridate)
library(tidyverse)
###############################################################################


# Modify test with weekly forecast
add_weekly_forecast <- function(current_test, sdept_preds, dept, model_num) {
  
  # gather Store, value, -Date on a (Date x #_store) forecast  
  sdept_preds <- sdept_preds %>% gather(Store, value, -Date, convert = T)
  
  pred.dept <- current_test %>% filter(Dept == dept) %>%
    select('Store', 'Date') %>% 
    left_join(sdept_preds, by = c('Store', 'Date'))
  
  pred.dept.idx <- current_test$Dept == dept
  
  pred.dept <- current_test[pred.dept.idx, c('Store', 'Date')] %>%
    left_join(sdept_preds, by = c('Store', 'Date'))
  
  if (model_num == 1)
    current_test$Weekly_Pred1[pred.dept.idx] <- pred.dept$value
  
  else if(model_num == 2)
    current_test$Weekly_Pred2[pred.dept.idx] <- pred.dept$value
  
  else
    current_test$Weekly_Pred3[pred.dept.idx] <- pred.dept$value
  
  current_test
}

# global test updated with mean of weekly predictions
finalize_test <- function(current_test) {
  
  # global test update
  test <<- test %>%
    
    left_join(current_test, by = c('Date', 'Store', 'Dept', 'IsHoliday')) %>%
    
    mutate(IsHoliday_Prev = IsHoliday) %>%
    mutate(Weekly_Pred3 = coalesce(Weekly_Pred3.y, Weekly_Pred3.x)) %>%
    mutate(Weekly_Pred1 = coalesce(Weekly_Pred1.y, Weekly_Pred1.x)) %>%
    mutate(Weekly_Pred2 = coalesce(Weekly_Pred2.y, Weekly_Pred2.x)) %>%
    select(-Weekly_Pred3.x, -Weekly_Pred3.y, -Weekly_Pred1.x, -Weekly_Pred1.y,
           -Weekly_Pred2.x, -Weekly_Pred2.y, -IsHoliday)
  
  test <<- test %>% rowwise() %>% 
    mutate(Weekly_Pred = ifelse(t == 1 | t >= 7,
                                mean(c(Weekly_Pred1, Weekly_Pred2, Weekly_Pred3)),
                                Weekly_Pred3))
}


##### Functions for Model Building  #####

#naive - naive simply takes last prediction to be reused.
my_naive_model<- function(train_ts, test_ts){
  
  num_forecasts <- nrow(test_ts)
  train_ts[is.na(train_ts)] <- 0 # remove NA's
  
  # forecasting using naive applied at per dept per store
  for(ts_col in 2:ncol(train_ts)){
    store_ts <- ts(train_ts[, ts_col], frequency=52)
    test_ts[, ts_col] <- naive(store_ts, num_forecasts)$mean
  }
  
  test_ts
}

#snaive - snaive simply takes last prediction from the same season to be reused.
my_snaive_model<- function(train_ts, test_ts){
  
  forecast_count <- nrow(test_ts)
  train_ts[is.na(train_ts)] <- 0 # remove NA's
  
  # forecasting using snaive applied at per dept per store
  for(ts_col in 2:ncol(train_ts)){
    store_ts <- ts(train_ts[, ts_col], frequency=52)
    test_ts[, ts_col] <- snaive(store_ts, forecast_count)$mean
  }
  
  test_ts
}

#tslm basic
my_tslm_basic_model <- function(train_ts, test_ts){
  
  forecast_count <- nrow(test_ts)
  train_ts[is.na(train_ts)] <- 0
  
  for(ts_col in 2:ncol(train_ts)){
    s <- ts(train_ts[, ts_col], frequency=52)
    tslm_model <- tslm(s ~ trend + season)
    new_forecast <- forecast(tslm_model, h=forecast_count)
    test_ts[, ts_col] <- as.numeric(new_forecast$mean)
  }
  
  test_ts
}

# Adjust sales over the last week of the year inorder to reduce the effect of weights = 5
fold5_shift_sales <- function(sdept_preds){
  
  base_threshold=1.1
  shift.days=1
  week.days=7
  
  new_ts <- ts(rep(0,39), frequency=52, start=c(2012,44))
  cycle_index <- cycle(new_ts) %in% 48:52
  
  holiday <- sdept_preds[cycle_index, 2:46]
  holiday[is.na(holiday)] <- 0
  
  baseline <- mean(rowMeans(holiday[c(1, 5), ], na.rm=T))
  surge <- mean(rowMeans(holiday[2:4, ], na.rm=T))
  
  sb = surge/baseline
  if(is.finite(sb) & sb > base_threshold){
    
    shifted.sales <- ((week.days-shift.days)/week.days) * holiday
    
    shifted.sales[2:5, ] <- shifted.sales[2:5, ] + (shift.days/week.days) * 
      holiday[1:4, ]
    
    shifted.sales[1, ] <- holiday[1, ]
    
    sdept_preds[cycle_index, 2:46] <- shifted.sales
  }
  
  sdept_preds
}

mypredict <- function() {
  
  if (t == 1) {
    test <<- test %>% mutate(test, Weekly_Pred1 = 0, Weekly_Pred2 = 0, Weekly_Pred3 = 0)
  } else {
    # append the previous periods test data to the current training data
    train <<- rbind(train, new_train)
    test <<- test %>% mutate(IsHoliday = IsHoliday_Prev) %>% select(-IsHoliday_Prev, -Weekly_Pred)
  }
  
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  
  current_test <- test %>% filter(Date >= start_date & Date < end_date)
  
  unique_test_dates <- unique(current_test$Date)
  unique_test_stores <- unique(current_test$Store)
  
  # Basic Test frame to hold department based weekly sale predictions
  test_frame <- data.frame(Date=rep(unique_test_dates, length(unique_test_stores)),
                           Store=rep(unique_test_stores, each=length(unique_test_dates)))
  
  unique_train_dates <- unique(train$Date)
  
  train_frame <- data.frame(Date=rep(unique_train_dates, length(unique_test_stores)), 
                            Store=rep(unique_test_stores, each=length(unique_train_dates)))
  
  # Unique Departments to iterate over
  test_depts <- unique(current_test$Dept)
  
  # Perform a individual forecasts for each department
  for (dept in test_depts) {
    
    # filter for the particular department in the training data
    train_dept_ts <- train %>% filter(Dept == dept) %>% select(Store, Date, Weekly_Sales)
    
    # dataframe contains (#_train_dates, #_stores)
    train_dept_ts <- train_frame %>% 
      left_join(train_dept_ts, by = c('Date', 'Store')) %>% 
      spread(Store, Weekly_Sales)
    
    # We create a similar dataframe to hold the forecasts on the dates in the testing window
    test_dept_ts <- test_frame %>% mutate(Weekly_Sales = 0) %>% spread(Store, Weekly_Sales)
    
    
    ###### Model Forecasting ######
    
    # model1: naive forecast
    f_naive <- my_naive_model(train_dept_ts, test_dept_ts)
    current_test <- add_weekly_forecast(current_test, sdept_preds=f_naive, dept, model_num = 1)
    
    # model2: snaive
    s_naive <- my_snaive_model(train_dept_ts, test_dept_ts)
    current_test <- add_weekly_forecast(current_test, sdept_preds=s_naive, dept, model_num = 2)
    
    # model3: tslm
    tslm_basic <- my_tslm_basic_model(train_dept_ts, test_dept_ts)
    if (t==5)
      tslm_basic <- fold5_shift_sales(sdept_preds=tslm_basic)
    current_test <- add_weekly_forecast(current_test, sdept_preds=tslm_basic, dept, model_num = 3)
    
  }
  
  # update global test dataframe
  finalize_test(current_test)
}
