library(tidyverse)

train.x <- read.csv('train.csv')
train.x <- select(train.x, - Sale_Price)
train.y <- log(read.csv('train.csv')$Sale_Price)

train.x # train data without "PID" and "Sale_Price"
train.y # log transformed "Sale_Price"

# replace missing by zero
train.x$Garage_Yr_Blt[is.na(train.x$Garage_Yr_Blt)] = 0

categorical.vars <- colnames(train.x)[
  which(sapply(train.x,
               function(x) mode(x)=="character"))]
train.matrix <- train.x[, !colnames(train.x) %in% categorical.vars, 
                        drop=FALSE]
n.train <- nrow(train.matrix)
for(var in categorical.vars){
  mylevels <- sort(unique(train.x[, var]))
  m <- length(mylevels)
  m <- ifelse(m>2, m, 1)
  tmp.train <- matrix(0, n.train, m)
  col.names <- NULL
  for(j in 1:m){
    tmp.train[train.x[, var]==mylevels[j], j] <- 1
    col.names <- c(col.names, paste(var, mylevels[j], sep=''))
  }
  colnames(tmp.train) <- col.names
  train.matrix <- cbind(train.matrix, tmp.train)
}