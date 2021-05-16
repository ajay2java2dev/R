###############################################################################
# Clean Workspace
rm(list = ls())

###############################################################################

## THIS IS AN NAIVE SOLUTION BASED ON note @279
mypredict_279_option1 <- function() {
  
  if (t > 1) {
    # append the previous periods test data to the current training data
    train <<- rbind(train, new_train)
  }
  
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  
  test_current <- test %>% filter(Date >= start_date & Date < end_date) %>% select(-IsHoliday)
  
  most_recent_date <- max(train$Date)
  
  tmp_train <- train %>% filter(Date == most_recent_date) %>% rename(Weekly_Pred = Weekly_Sales) %>% select(-Date, -IsHoliday)
  
  test_pred <- test_current %>% left_join(tmp_train, by=c('Dept', 'Store'))
  
  return (test_pred)
}

#> print(wae)
#[1] 2078.726 2589.338 2253.936 2823.098 5156.012 4218.348 2269.904 2143.839 2221.145 2372.425
#> mean(wae)
#[1] 2812.677
