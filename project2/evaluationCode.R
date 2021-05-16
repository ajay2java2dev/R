basic.packages = c("package:stats",
                   "package:graphics",
                   "package:grDevices",
                   "package:utils",
                   "package:datasets",
                   "package:methods",
                   "package:base")
package.list = search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
package.list = setdiff(package.list,basic.packages)
if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)


source("mymain.R")

# read in train / test dataframes
train <- readr::read_csv('train_ini.csv')
test <- readr::read_csv('test.csv')

# save weighted mean absolute error WMAE
num_folds <- 10
wae <- rep(0, num_folds)

test.start.time <- proc.time()

for (t in 1:num_folds) {
  fold.start.time <- proc.time()
  
  # *** THIS IS YOUR PREDICTION FUNCTION ***
  test_pred <- mypredict()

  # load fold file 
  fold_file <- paste0('fold_', t, '.csv')
  new_train <- readr::read_csv(fold_file, 
                               col_types = cols())
  
  # extract predictions matching up to the current fold
  scoring_tbl <- new_train %>% 
    left_join(test_pred, by = c('Date', 'Store', 'Dept'))

  # compute WMAE
  actuals <- scoring_tbl$Weekly_Sales
  preds <- scoring_tbl$Weekly_Pred
  preds[is.na(preds)] <- 0
  weights <- if_else(scoring_tbl$IsHoliday, 5, 1)
  #wae[t] <- sum(weights * abs(actuals - preds)) / sum(weights)
  
  test.runtime <- (proc.time() - fold.start.time)
  test.runtime
  print(paste("fold", t, test.runtime[[3]]))
  
  wae[t] <- sum(weights * abs(actuals - preds)) / sum(weights)
}

test.runtime <- (proc.time() - test.start.time)
test.runtime
test.runtime[[3]]

print(wae)
mean(wae)
