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

#####################################
# FIXME :Remove the code once implementation complete
#####################################
getwd() # should be in project working directory.

j = 1 # FIXME: remove this line later.
setwd(paste("split_",j, sep="")) # FIXME: remove this line later.

#####################################
# Load train and test data first.
#####################################

train <- read.table("train.tsv", stringsAsFactors = FALSE, header = TRUE)
train$review <- gsub('<.*?>', ' ', train$review) # clean html tags
     
test  <- read.table("test.tsv", stringsAsFactors = FALSE, header = TRUE)
test$review <- gsub('<.*?>', ' ', train$review) # clean html tags

setwd("..") # FIXME: remove this line later. Move back to parent folder
getwd() # should be in project working directory.

#####################################
# Load your vocabulary and training data
#####################################

myvocab <- scan(file = "myvocab.txt", what = character())

tokens = itoken(myvocab, preprocessor = tolower,tokenizer = word_tokenizer)

stop_words = c("i", "me", "my", "myself", 
               "we", "our", "ours", "ourselves", 
               "you", "your", "yours", 
               "their", "they", "his", "her", 
               "she", "he", "a", "an", "and",
               "is", "was", "are", "were", 
               "him", "himself", "has", "have", 
               "it", "its", "of", "one", "for", 
               "the", "us", "this")

myvocab = create_vocabulary(tokens, ngram = c(1L, 4L), stopwords = stop_words)

#####################################
#
# Train a binary classification model
#
#####################################
#tmp.term =  gsub(" ","_", myvocab)

#myvocab = myvocab[myvocab$term %in% tmp.term, ]

bigram_vectorizer = vocab_vectorizer(myvocab)

it_train = itoken(train$review, preprocessor = tolower, tokenizer = word_tokenizer)

dtm_train = create_dtm(it_train, bigram_vectorizer)

it_test = itoken(test$review, preprocessor = tolower, tokenizer = word_tokenizer)

dtm_test = create_dtm(it_test, bigram_vectorizer)



#####################################
# Compute prediction 
# Store your prediction for test data in a data frame
# "output": col 1 is test$id
#           col 2 is the predited probabilities
#####################################


length(train$sentiment)

set.seed(1234)

n_fold = 10

mycv = cv.glmnet(x=dtm_train, y=train$sentiment, family='binomial',type.measure = "auc", 
                 nfolds = n_fold, alpha=0)

myfit = glmnet(x=dtm_train, y=train$sentiment, 
               lambda = mycv$lambda.min, family='binomial', alpha=0)

logit_pred = predict(myfit, dtm_test, type = "response")

output = data.frame("id" = test$id, "prob" = logit_pred[,1])

head(output)

write.table(output, file = "mysubmission.txt", row.names = FALSE)


