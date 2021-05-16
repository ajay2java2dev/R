rm(list=ls())

library(pROC)
source(mymain_ajay.R) # no method calls in mymain.R

# move test_y.tsv to this directory

j = 1 # FIXME: remove this line later.
setwd(paste("split_",j, sep="")) # FIXME: remove this line later.

test.y <- read.table("test_y.tsv", header = TRUE)

setwd("..") # FIXME: remove this line later.

pred <- read.table("mysubmission.txt", header = TRUE)
pred <- merge(pred, test.y, by="id")
roc_obj <- roc(pred$sentiment, pred$prob)
pROC::auc(roc_obj)