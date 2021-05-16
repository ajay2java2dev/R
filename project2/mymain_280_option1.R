###############################################################################
# Clean Workspace
rm(list = ls())

###############################################################################

## THIS IS AN NAIVE SOLUTION BASED ON note @279
mypredict_280_option1 <- function() {
  
  if (t > 1) {
    # append the previous periods test data to the current training data
    train <<- rbind(train, new_train)
  }
  
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  
  # The below is all same for weeks 2010 to 2012. Not clear why 2010 is 53 weeks since even for 2011 and 2012 its 53
  #week("2010-12-01") # Week 48
  #week("2010-12-26") # Week 52
  #week("2010-12-31") # Week 53
  
  #week("2011-12-01") # Week 48
  #week("2011-12-26") # Week 52
  #week("2011-12-31") # Week 53
  
  #week("2012-12-01") # Week 48
  #week("2012-12-26") # Week 52
  #week("2012-12-31") # Week 53
  
  test_current <- test %>% filter(Date >= start_date & Date < end_date) %>% select(-IsHoliday)
  
  
  start_last_year = min(test_current$Date) - 375
  end_last_year = max(test_current$Date) - 350
  
  
  
  tmp_train <- train %>% 
    
    filter(Date > start_last_year & Date < end_last_year) %>%
    mutate(Wk = ifelse(year(Date) == 2010, week(Date) - 1, week(Date))) %>%
    rename(Weekly_Pred = Weekly_Sales) %>% select(-Date, -IsHoliday)
  
  
  test_current <- test_current %>% mutate(Wk = week(Date))
    
  test_pred <- test_current %>% left_join(tmp_train, by=c('Dept', 'Store', 'Wk')) %>% select(-Wk)
  
  return (test_pred)
}


#> print(wae)
#[1] 2262.422 1787.081 1779.052 1716.117 2400.395 1696.900 2086.967 1750.283 1719.887 1680.956
#> mean(wae)
#[1] 1888.006