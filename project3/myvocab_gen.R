####################
'
Col 1: "id", the identification number;
Col 2: "sentiment", 0 = negative and 1 = positive;
Col 3: "score", the 10-point score assigned by the reviewer. Scores 1-4 correspond to negative sentiment; Scores 7-10 corrspond to positive sentiment. This dataset contains no reviews with score 5 or 6.
Col 4: "review".
'
###################

library(pROC)
library(glmnet)
library(text2vec)
library(slam)
library(stopwords)
library(dplyr)

rm(list = ls())
myvocab_unique_list = NA
set.seed(1234)

for (j in 1:5) {
  #j = 3
  
  #####################################
  # Load train set from each split and move back to parent folder
  #####################################
  setwd(paste("split_", j, sep=""))
  train <- read.table("train.tsv", stringsAsFactors = FALSE, header = TRUE)
  setwd("..")
  train$review <- gsub('<.*?>', ' ', train$review) # clean html tags
  #####################################
  
  #stop_words = stopwords("en")
  
  stop_words = c("i", "me", "my", "myself", 
                 "we", "our", "ours", "ourselves", 
                 "you", "your", "yours", 
                 "their", "they", "his", "her", 
                 "she", "he", "a", "an", "and",
                 "is", "was", "are", "were", 
                 "him", "himself", "has", "have", 
                 "it", "its", "of", "one", "for", 
                 "the", "us", "this")
  
  it_train = itoken(train$review,
                    preprocessor = tolower, 
                    tokenizer = word_tokenizer)
  #?create_vocabulary
  #?prune_vocabulary
  
  tmp.vocab = create_vocabulary(it_train, 
                                stopwords = stop_words, 
                                ngram = c(1L,2L))
  tmp.vocab = prune_vocabulary(tmp.vocab, term_count_min = 10,
                               doc_proportion_max = 0.5,
                               doc_proportion_min = 0.001)
  dtm_train  = create_dtm(it_train, vocab_vectorizer(tmp.vocab))
  
  v.size = dim(dtm_train)[2]
  ytrain = train$sentiment
  
  summ = matrix(0, nrow=v.size, ncol=4)
  summ[,1] = colapply_simple_triplet_matrix(
    as.simple_triplet_matrix(dtm_train[ytrain==1, ]), mean)
  summ[,2] = colapply_simple_triplet_matrix(
    as.simple_triplet_matrix(dtm_train[ytrain==1, ]), var)
  summ[,3] = colapply_simple_triplet_matrix(
    as.simple_triplet_matrix(dtm_train[ytrain==0, ]), mean)
  summ[,4] = colapply_simple_triplet_matrix(
    as.simple_triplet_matrix(dtm_train[ytrain==0, ]), var)
  
  n1 = sum(ytrain); 
  n = length(ytrain)
  n0 = n - n1
  
  myp = (summ[,1] - summ[,3])/sqrt(summ[,2]/n1 + summ[,4]/n0)
  
  # Order the top 2000 words based on their t-statistics value
  id = order(abs(myp), decreasing=TRUE)[1:2000]
  words = colnames(dtm_train)
  pos.list = words[id[myp[id]>0]]
  neg.list = words[id[myp[id]<0]]
  
  # Use Lasso to reduce the vocabulary down to 1000 words or less
  tmpfit = glmnet(x = dtm_train[,id], 
                  y = train$sentiment, 
                  alpha = 1,
                  family='binomial')
  tmpfit$df
  # Column no. 41 has the highest value under 1000, i.e. 961
  myvocab = colnames(dtm_train[,id])[which(tmpfit$beta[, 32] != 0)]
  
  myvocab_unique_list <- c(myvocab_unique_list, myvocab)
  
}

myvocab_unique_list <- unique(na.omit(myvocab_unique_list))

# deal with special characters
#myvocab_unique_list <- gsub('<.*?>', ' ', myvocab_unique_list)
#myvocab_unique_list <- gsub('<U+FFFD>', '', myvocab_unique_list)
#myvocab_unique_list <- gsub('_', ' ', myvocab_unique_list)
#myvocab_unique_list <- gsub(' ?', '', myvocab_unique_list)


write.table(myvocab_unique_list, file = "mynewvocab.txt", row.names = FALSE, 
            col.names = FALSE, sep = "\n")



