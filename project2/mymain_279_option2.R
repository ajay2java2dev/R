###############################################################################
# Clean Workspace
rm(list = ls())

###############################################################################

## THIS IS AN NAIVE SOLUTION BASED ON note @279
mypredict_279_option2 <- function() {
  
  if (t > 1) {
    # append the previous periods test data to the current training data
    train <<- rbind(train, new_train)
  }
  
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  
  test_current <- test %>% filter(Date >= start_date & Date < end_date) %>% select(-IsHoliday)
  
  #most_recent_date <- max(train$Date)
  
  tmp_train <- train %>% 
    group_by(Dept,Store) %>% 
    filter(Date == max(Date)) %>% # group within
    ungroup() %>%
    rename(Weekly_Pred = Weekly_Sales) %>% select(-Date, -IsHoliday)
  
  test_pred <- test_current %>% left_join(tmp_train, by=c('Dept', 'Store'))
  
  return (test_pred)
}


#> print(wae)
#[1] 2080.420 2589.082 2253.804 2822.989 5155.406 4219.021 2277.086 2143.963 2221.037 2372.435
#> mean(wae)
#[1] 2813.524