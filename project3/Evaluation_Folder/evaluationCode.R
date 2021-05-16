# TODO: Remove before submission
rm(list = ls())

library(pROC)

outputs <- matrix(nrow=5, ncol=2)
start.time <- proc.time()

merged <- na.omit(data.frame("id" = NA, "prob" = NA, "sentiment" = NA, "score" = NA, "review" = NA))

overall.time <- 0

for (j in 1:5) {
  split.start.time <- proc.time()
  
  # move train.tsv to this directory
  path <- paste("../split_", j, sep="")
  print(path)
  file.copy(file.path(path, "train.tsv"), "./")
  # move test.tsv to this directory
  file.copy(file.path(path, "test.tsv"), "./")
  
  source("mymain.R")
  
  # move test_y.tsv to this directory
  file.copy(file.path(path, "test_y.tsv"), "./")
  
  test.y <- read.table("test_y.tsv", header = TRUE)
  pred <- read.table("mysubmission.txt", header = TRUE)
  pred <- merge(pred, test.y, by="id")
  
  # save the merged file for debugging
  test <- read.table("test.tsv", header = TRUE)
  pred.full <- merge(pred, test, by="id")
  write.table(pred.full, 
              file = paste("merged_", j, ".csv", sep = ""), 
              row.names = FALSE, 
              col.names = TRUE,
              sep = "\t")
  print(head(pred))
  merged <- rbind(merged, pred.full)
  
  # get the AUC score
  roc_obj <- roc(pred$sentiment, pred$prob)
  print(pROC::auc(roc_obj))
  outputs[j, 1] <- round(pROC::auc(roc_obj), 4)
  
  # get the computation time
  split.duration <- proc.time() - split.start.time
  print(split.duration)
  outputs[j, 2] <- round(split.duration[[3]], 3)
  overall.time <- overall.time + round(split.duration[[3]], 3)
  
  # clean up for the next run
  unlink("train.tsv")
  file.remove("test.tsv")
  file.remove("test_y.tsv")
}

merged.ordered.pos <- merged[order(abs(merged[,"prob"]), decreasing = TRUE),]
write.table(merged.ordered.pos, 
            file = "merged_pos_ordered.csv",
            row.names = FALSE,
            col.names = TRUE,
            sep = "\t")

merged.ordered.neg <- merged[order(abs(merged[,"prob"]), decreasing = FALSE),]
write.table(merged.ordered.neg, 
            file = "merged_neg_ordered.csv",
            row.names = FALSE,
            col.names = TRUE,
            sep = "\t")


# # Write negative reviews in worst to best order
# merged.0 <- merged[(merged[,"sentiment"] == 0),]
# merged.0.ordered <- merged.0[order(abs(merged.0[,"prob"]), decreasing = FALSE),]
# write.table(merged.0, 
#             file = "merged_neg.csv", 
#             row.names = FALSE, 
#             col.names = TRUE,
#             sep = "\t")
# write.table(merged.0.ordered, 
#             file = "merged_neg_ordered.csv", 
#             row.names = FALSE, 
#             col.names = TRUE,
#             sep = "\t")
# 
# 
# # Write positive reviews in best to worst order
# merged.1 <- merged[(merged[,"sentiment"] == 1),]
# merged.1.ordered <- merged.1[order(abs(merged.1[,"prob"]), decreasing = TRUE),]
# write.table(merged.1, 
#             file = "merged_pos.csv", 
#             row.names = FALSE, 
#             col.names = TRUE,
#             sep = "\t")
# write.table(merged.1.ordered, 
#             file = "merged_pos_ordered.csv", 
#             row.names = FALSE, 
#             col.names = TRUE,
#             sep = "\t")


write.table(outputs, 
            file = "outputs.csv", 
            row.names = FALSE, 
            col.names = FALSE)

#print(paste0("Overall time :", (proc.time() - start.time)[[3]]))
print(paste0("Overall time :", round(overall.time, 3)))
