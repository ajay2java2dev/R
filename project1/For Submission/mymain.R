# Project 1: Predict housing Prices in Ames Dataset
# Authors: Team MAK:
#           - Mriganka Sarma: ms76
#           - Ajay Menon: kamenon2
#           - Kai Pak: kaipak2
#

# Step 0: Load necessary libraries
###########################################
library(tidyverse)
library(dplyr)
library(reshape2)
library(glmnet)
library(gbm)
library(xgboost)
library(ggplot2)
library(caret)
library(randomForest)

###########################################
# Step 1: Preprocess training data
#         and fit two models
#
PreprocessData <- function(df) {
  remove.var <- c('PID', 'Street', 'Utilities', 'Condition_2', 'Roof_Matl', 
                  'Heating', 'Pool_QC', 'Misc_Feature', 'Low_Qual_Fin_SF', 
                  'Pool_Area', 'Longitude','Latitude')
  df <- df %>% select(-remove.var)
  
  winsor.vars <- c("Lot_Frontage", "Lot_Area", "Mas_Vnr_Area", "BsmtFin_SF_2", 
                   "Bsmt_Unf_SF", "Total_Bsmt_SF", "Second_Flr_SF", 
                   'First_Flr_SF', "Gr_Liv_Area", "Garage_Area", "Wood_Deck_SF", 
                   "Open_Porch_SF", "Enclosed_Porch", "Three_season_porch", 
                   "Screen_Porch", "Misc_Val")
  quan.value <- 0.95
  for(var in winsor.vars){
    tmp <- df[, var]
    myquan <- quantile(tmp, probs = quan.value, na.rm = TRUE)
    tmp[tmp > myquan] <- myquan
    df[, var] <- tmp
  }
  
  # Replace text ratings with numerical ones. 
  df$Overall_Qual_Score[df$Overall_Qual == "Very_Poor"] <- 1 
  df$Overall_Qual_Score[df$Overall_Qual == "Poor"] <- 2 
  df$Overall_Qual_Score[df$Overall_Qual == "Fair"] <- 3 
  df$Overall_Qual_Score[df$Overall_Qual == "Below_Average"] <- 4 
  df$Overall_Qual_Score[df$Overall_Qual == "Average"] <- 5 
  df$Overall_Qual_Score[df$Overall_Qual == "Above_Average"] <- 6 
  df$Overall_Qual_Score[df$Overall_Qual == "Good"] <- 7 
  df$Overall_Qual_Score[df$Overall_Qual == "Very_Good"] <- 8 
  df$Overall_Qual_Score[df$Overall_Qual == "Excellent"] <- 9 
  df$Overall_Qual_Score[df$Overall_Qual == "Very_Excellent"] <- 10
  df <- df %>% select(-Overall_Qual)
  df$Overall_Cond_Score[df$Overall_Cond == "Very_Poor"] <- 1 
  df$Overall_Cond_Score[df$Overall_Cond == "Poor"] <- 2 
  df$Overall_Cond_Score[df$Overall_Cond == "Fair"] <- 3 
  df$Overall_Cond_Score[df$Overall_Cond == "Below_Average"] <- 4 
  df$Overall_Cond_Score[df$Overall_Cond == "Average"] <- 5 
  df$Overall_Cond_Score[df$Overall_Cond == "Above_Average"] <- 6 
  df$Overall_Cond_Score[df$Overall_Cond == "Good"] <- 7 
  df$Overall_Cond_Score[df$Overall_Cond == "Very_Good"] <- 8 
  df$Overall_Cond_Score[df$Overall_Cond == "Excellent"] <- 9 
  df$Overall_Cond_Score[df$Overall_Cond == "Very_Excellent"] <- 10
  df <- df %>% select(-Overall_Cond)
  
  dmy <- dummyVars("~ .", data = df, fullRank = F)
  df <- data.frame(predict(dmy, newdata = df))
  
  # Deal with missing Garage_Yr_Blt problem
  df <- df %>% mutate(
    Garage_Yr_Blt = ifelse(is.na(Garage_Yr_Blt), 0, Garage_Yr_Blt)
  )
  
  # Ames data set was collected b/w 2006 to 2010 originally
  row_gt_2010 <- which (df$Garage_Yr_Blt > 2010) 
  df$Garage_Yr_Blt[row_gt_2010] = 2007
  
  return(df)
}

AlignDummies <- function(train, test) {
  # Generating dummies will often lead to mismatched design matrices. This 
  # fixes missing parameters.
  
  # Columns missing in test but in train
  missing.te.cols <- names(train)[!(names(train) %in% names(test))]
  test[, missing.te.cols] <- 0
  
  # Drop columns missing in train but in test
  missing.tr.cols <- names(test)[!(names(test) %in% names(train))]
  test <- test[, !colnames(test) %in% missing.tr.cols]
  
  return(test)
}

EngineerFeatures <- function(df) {
  df$Bath_to_Bedroom <- ifelse(df$Bedroom_AbvGr > 0, 
                               (df$Bsmt_Full_Bath + 
                                df$Bsmt_Half_Bath + 
                                df$Full_Bath + 
                                df$Half_Bath) / 
                                df$Bedroom_AbvGr, 0)
  df$Bedroom_to_Gr_Liv_Area <- df$Bedroom_AbvGr / df$Gr_Liv_Area
  df$Has_Basement <- as.numeric(df$Total_Bsmt_SF > 0)
  
  return(df)
}

###########################################
# Step 1: Preprocess training data
#         and fit two models
#

mysubmission1 = list(); mysubmission2 = list();
train.x <- read.csv("train.csv")
train.y <- log(train.x$Sale_Price)
train.x <- train.x %>% select(-'Sale_Price')
train.x <- PreprocessData(train.x)
train.x <- EngineerFeatures(train.x)
train.x <- train.x[, order(names(train.x))]

# ElasticNet
model1.train.elastic = cv.glmnet(as.matrix(train.x), train.y, alpha=0.5)

# XGBoost
model2.train.xgb <- xgboost(data = as.matrix(train.x), 
                            label = train.y, max_depth = 6, 
                            eta = 0.05, nrounds = 5000, 
                            subsample = 0.7, verbose = FALSE)

###########################################
# Step 2: Preprocess test data
#         and output predictions into two files
#
test.x <- read.csv("test.csv")
test.PID <- test.x$PID
test.x <- PreprocessData(test.x)
test.x <- AlignDummies(train.x, test.x)
test.x <- EngineerFeatures(test.x)
test.x <- test.x[, order(names(test.x))]

model1.yhat.test <- predict(model1.train.elastic, 
                            s=model1.train.elastic$lambda.min, 
                            as.matrix(test.x))

model2.yhat.test <- predict(model2.train.xgb, as.matrix(test.x))


mysubmission1 <- rbind(mysubmission1, 
                       data.frame(PID=test.PID, 
                                  Sale_Price=exp(model1.yhat.test)))
mysubmission2 <- rbind(mysubmission2, 
                       data.frame(PID=test.PID, 
                                  Sale_Price=exp(model2.yhat.test)))

write.table(mysubmission1, "mysubmission1.txt", 
            sep = ",", row.names = FALSE, col.names = c("PID", "Sale_Price"))
write.table(mysubmission2, "mysubmission2.txt", 
            sep = ",", row.names = FALSE, col.names = c("PID", "Sale_Price"))