###############################################################################
# Clean Workspace
rm(list = ls())

###############################################################################

## THIS IS AN NAIVE SOLUTION BASED ON note @279
mypredict_281_option1 <- function() {
  
  if (t > 1) {
    # append the previous periods test data to the current training data
    train <<- rbind(train, new_train)
  }
  
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  
  test_current <- test %>% filter(Date >= start_date & Date < end_date) %>% select(-IsHoliday)
  test_depts <- unique(test_current$Dept)
  test_pred <- NULL
  
  for (dept in test_depts) {
    #dept = 1
    train_dept_data <- train %>% filter(Dept == dept)
    test_dept_data <- test_current %>% filter(Dept == dept)
    
    # no need to consider stores that do not need prediction or do not have training samples
    train_stores <- unique(train_dept_data$Store)
    test_stores <-  unique(test_dept_data$Store)
    test_stores <-  intersect(train_stores, test_stores)
    
    for (store in test_stores) {
      #store =1 
      tmp_train <- train_dept_data %>% filter(Store == store) %>%
        mutate(Wk = ifelse(year(Date) == 2010, week(Date) - 1, week(Date))) %>%
        mutate(Yr = year(Date))
      
      tmp_test <- test_dept_data %>% filter(Store == store) %>%
        mutate(Wk = ifelse(year(Date) == 2010, week(Date) - 1, week(Date))) %>%
        mutate(Yr = year(Date))
      
      tmp_train$Wk = factor (tmp_train$Wk, levels = 1:52)
      tmp_test$Wk = factor (tmp_test$Wk, levels = 1:52)
      
      train_model_matrix <- model.matrix(~ Yr + Wk, tmp_train)
      test_model_matrix <- model.matrix(~ Yr + Wk, tmp_test)
      
      mycoef <- lm(tmp_train$Weekly_Sales ~ train_model_matrix)$coef
      mycoef[is.na(mycoef)] <- 0
      
      tmp_pred <- mycoef[1] + test_model_matrix %*% mycoef[-1]
      
      tmp_test <- tmp_test %>%
        mutate(Weekly_Pred = tmp_pred[,1]) %>%
        select(-Wk, -Yr)
      
      test_pred <- test_pred %>% bind_rows(tmp_test)
    }
    
  }
    
  return (test_pred)
}


#> print(wae)
#[1] 2045.243 1466.912 1449.852 1593.998 2324.496 1677.483 1722.274 1428.212 1443.960 1444.656
#> mean(wae)
#[1] 1659.709