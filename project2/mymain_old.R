###############################################################################
# Clean Workspace
rm(list = ls())

###############################################################################
# Library

if (!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr", "tidyr", "reshape2","data.table", "forecast", "lubridate", "tidyverse")
###############################################################################

# converts a Date x num_store forecast to a dataframe
# with Date, Store, value = Weekly_Price columns
flatten_forecast <- function(f_model) {
  f_model %>% gather(Store, value, -Date, convert = TRUE)
}

# Adds forecasts to the testing dataframe
update_forecast <- function(test_current, dept_preds, dept, num_model) {
  
  dept_preds <- flatten_forecast(dept_preds)
  
  pred.d <- test_current %>%
    filter(Dept == dept) %>%
    select('Store', 'Date') %>%
    left_join(dept_preds, by = c('Store', 'Date'))
  
  pred.d.idx <- test_current$Dept == dept
  pred.d <- test_current[pred.d.idx, c('Store', 'Date')] %>%
    left_join(dept_preds, by = c('Store', 'Date'))
  
  if (num_model == 1) {
    
    test_current$Weekly_Pred1[pred.d.idx] <- pred.d$value
    
  } else if(num_model == 2) {
    
    test_current$Weekly_Pred2[pred.d.idx] <- pred.d$value
    
  } else {
    
    test_current$Weekly_Pred3[pred.d.idx] <- pred.d$value
    
  }
  
  test_current
}

# update forecasts in the global test dataframe
update_test_old <- function(test_current) {
  test <<- test %>%
    dplyr::left_join(test_current,
                     by = c('Date', 'Store', 'Dept', 'IsHoliday')) %>%
    mutate(Weekly_Pred1 = coalesce(Weekly_Pred1.y, Weekly_Pred1.x)) %>%
    mutate(Weekly_Pred2 = coalesce(Weekly_Pred2.y, Weekly_Pred2.x)) %>%
    mutate(Weekly_Pred3 = coalesce(Weekly_Pred3.y, Weekly_Pred3.x)) %>%
    select(-Weekly_Pred1.x, -Weekly_Pred1.y,
           -Weekly_Pred2.x, -Weekly_Pred2.y,
           -Weekly_Pred3.x, -Weekly_Pred3.y)
}

##FIXME: This is a modified version of above to fit the evaluationCode. Can be better.
update_test <- function(test_current) {
  
  test <<- test %>%
    dplyr::left_join(test_current,
                     by = c('Date', 'Store', 'Dept', 'IsHoliday')) %>%
    mutate(Weekly_Pred1 = coalesce(Weekly_Pred1.y, Weekly_Pred1.x)) %>%
    mutate(Weekly_Pred2 = coalesce(Weekly_Pred2.y, Weekly_Pred2.x)) %>%
    mutate(Weekly_Pred3 = coalesce(Weekly_Pred3.y, Weekly_Pred3.x)) %>%
    
    mutate(IsHoliday_Prev = IsHoliday) %>%
    select(-Weekly_Pred1.x, -Weekly_Pred1.y,
           -Weekly_Pred2.x, -Weekly_Pred2.y,
           -Weekly_Pred3.x, -Weekly_Pred3.y, -IsHoliday)
  
  test <<- test %>% rowwise() %>% mutate(Weekly_Pred = mean(c(Weekly_Pred1, Weekly_Pred2, Weekly_Pred3)))
    
}


##### Model Building Functions #####

# Forecasts out the last observation in the training data
naive_model<- function(train_ts, test_ts){
  num_forecasts <- nrow(test_ts)
  train_ts[is.na(train_ts)] <- 0
  
  # naive forecast per store
  for(j in 2:ncol(train_ts)){
    store_ts <- ts(train_ts[, j], frequency=52)
    test_ts[, j] <- naive(store_ts, num_forecasts)$mean
  }
  test_ts
}

#tslm with svd
tslm.svd <- function(train, test, n.comp){
  # Computes a forecast using linear regression and seasonal dummy variables
  #
  # args:
  # train - A matrix of Weekly_Sales values from the training set of dimension
  #         (number of weeeks in training data) x (number of stores)
  # test - An all-zeros matrix of dimension:
  #       (number of weeeks in training data) x (number of stores)
  #       The forecasts are written in place of the zeros.
  #
  # returns:
  #  the test(forecast) data frame with the forecasts filled in 
  horizon <- nrow(test)
  train <- preprocess.svd(train, n.comp) 
  for(j in 2:ncol(train)){
    s <- ts(train[, j], frequency=52)
    model <- tslm(s ~ trend + season)
    fc <- forecast(model, h=horizon)
    test[, j] <- as.numeric(fc$mean)
  }
  test
}

#stlf
stlf.svd <- function(train, test, model.type, n.comp){
  # Replaces the training data with a rank-reduced approximation of itself,
  # then forecasts each store using stlf() from the forecast package.
  # That function performs an STL decomposition on each series, seasonally
  # adjusts the data, non-seasonally forecasts the seasonally adjusted data,
  # and then adds in the naively extended seasonal component to get the
  # final forecast.
  #
  # args:
  # train - A matrix of Weekly_Sales values from the training set of dimension
  #         (number of weeeks in training data) x (number of stores)
  # test - An all-zeros matrix of dimension:
  #       (number of weeeks in training data) x (number of stores)
  #       The forecasts are written in place of the zeros.
  # model.type - one of 'ets' or 'arima', specifies which type of model to
  #        use for the non-seasonal forecast
  # n.comp - the number of components to keep in the singular value
  #         decomposition that is performed for preprocessing
  #
  # returns:
  #  the test(forecast) data frame with the forecasts filled in 
  horizon <- nrow(test)
  train <- preprocess.svd(train, n.comp) 
  for(j in 2:ncol(train)){
    s <- ts(train[, j], frequency=52)
    if(model.type == 'ets'){
      fc <- stlf(s, 
                 h=horizon, 
                 s.window=3, 
                 method='ets',
                 ic='bic', 
                 opt.crit='mae')
    }else if(model.type == 'arima'){
      fc <- stlf(s, 
                 h=horizon, 
                 s.window=3, 
                 method='arima',
                 ic='bic')
    }else{
      stop('Model type must be one of ets or arima.')
    }
    pred <- as.numeric(fc$mean)
    test[, j] <- pred
  }
  test
}
preprocess.svd <- function(train, n.comp){
  # Replaces the training data with a rank-reduced approximation of itself.
  # This is for noise reduction. The intuition is that characteristics
  # that are common across stores (within the same department) are probably
  # signal, while those that are unique to one store may be noise.
  #
  # args:
  # train - A matrix of Weekly_Sales values from the training set of dimension
  #         (number of weeeks in training data) x (number of stores)
  # n.comp - the number of components to keep in the singular value
  #         decomposition
  #
  # returns:
  #  the rank-reduced approximation of the training data
  train[is.na(train)] <- 0
  z <- svd(train[, 2:ncol(train)], nu=n.comp, nv=n.comp)
  s <- diag(z$d[1:n.comp])
  train[, 2:ncol(train)] <- z$u %*% s %*% t(z$v)
  train
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
  
  test_current <- test %>% filter(Date >= start_date & Date < end_date)
  
  # Dates are not the same across months!
  test_dates <- unique(test_current$Date)
  num_test_dates <- length(test_dates)
  
  # Unique Test Stores
  all_stores <- unique(test_current$Store)
  num_stores <- length(all_stores)
  
  # Unique Test Departments
  test_depts <- unique(test_current$Dept)
  
  # Test Data frame
  test_frame <- data.frame( Date=rep(test_dates, num_stores),Store=rep(all_stores, each=num_test_dates))
  
  # Train Dates
  train_dates <- unique(train$Date)
  num_train_dates <- length(train_dates)
  train_frame <- data.frame(Date=rep(train_dates, num_stores), Store=rep(all_stores, each=num_train_dates))
  
  #### Perform a individual forecasts for each department
  for (dept in test_depts) {
    
    #dept = 1
    
    # filter for the particular department in the training data
    train_dept_ts <- train %>% filter(Dept == dept) %>% select(Store, Date, Weekly_Sales)
    
    # Reformat so that each column is a weekly time-series for that store's department.
    # The dataframe has a shape (num_train_dates, num_stores)
    train_dept_ts <- train_frame %>% left_join(train_dept_ts, by = c('Date', 'Store')) %>% spread(Store, Weekly_Sales)
    
    # We create a similar dataframe to hold the forecasts on the dates in the testing window
    test_dept_ts <- test_frame %>% mutate(Weekly_Sales = 0) %>% spread(Store, Weekly_Sales)
    
    ###### Model Fitting / Forecasting ######
    
    
    # model1: naive forecast
    f_naive <- naive_model(train_dept_ts, test_dept_ts)
    test_current <- update_forecast(test_current, f_naive, dept, 1)
    
    # model2: tslm with svd forecast
    f_tslm <- tslm.svd(train_dept_ts, test_dept_ts,12)
    test_current <- update_forecast(test_current, f_tslm, dept, 2)
    
    
    # model3: tslm with svd forecast + stlf for last two folds
    test_current <- update_forecast(test_current, f_tslm, dept, 3)
    if (start_date > ymd("2012-02-28")){
      f_stlf <- stlf.svd(train_dept_ts, test_dept_ts,'ets' ,12)
      test_current <- update_forecast(test_current, f_stlf, dept, 3)
    }
  }
  
  # update global test dataframe
  update_test(test_current)
}