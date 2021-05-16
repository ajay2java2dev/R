### CHECKOUT THIS VIDEO ON MEAN NORMALIZATION - https://youtu.be/Am9fhp2Q91o?list=PLLssT5z_DsK-h9vYZkQkYNWcItqhlRJLN

library(recommenderlab)
data("Jester5k")

######## CALCULATION USING RECOMMENDER LIBRARY #################

# Let's illustrate this using a small rating matrix from 200 users and 100 items.
small.data = Jester5k[1:200]
nratings(small.data)

?realRatingMatrix
mydata = as(small.data, "matrix")
t_small = mydata[1:20,1:20] # note the utility matrix looks like u{ID} format - sample in ead
mean(na.omit(t_small['u238',]))

#Use 'POPULAR' to predict missing entries in mydata. 
#You'll find that the prediction for the same item is different from user to user.
rec_popular = Recommender(small.data, method="POPULAR") # for example: u238 has j3,j4 etc as NA's
recom = predict(rec_popular, small.data, type = 'ratings')

myout = as(recom, "matrix") # notice what happens to other columns and also to u238 -->> j3,j4 etc columns.
(myout['u238',1:20]) # note how the NA column has been replaced with center normalized values
cbind(mydata[1:20, 1:5], round(myout[1:20, 1:5], dig=2))

getModel(rec_popular) # normalize = center


####### MANUAL CALCULATION ############
#Let's compute the prediction of 'POPULAR' using our own code.

#1) Compute the centered rating matrix: remove the row mean from each row; ignore NA entries.
n.user = nrow(mydata)
n.item = ncol(mydata)
item.names = colnames(mydata)
user.mean = rowMeans(mydata, na.rm=TRUE)
norm.data = mydata

for(i in 1:n.user){
  tmp.id = which(is.na(mydata[i,]) == FALSE)
  norm.data[i, tmp.id] = mydata[i, tmp.id] - user.mean[i]
}

#2) Compute the 'POPULAR' prediction for each movie by a simple column average. Again, ignore NA entries.
item.mean = colMeans(norm.data, na.rm=TRUE)

#3) Predict the NA entries by item.mean + user.mean
mypred = matrix(NA, n.user, n.item)
colnames(mypred) = item.names

for(i in 1:n.user){
  tmp.id = which(is.na(mydata[i,]) ==TRUE)
  mypred[i, tmp.id] = item.mean[tmp.id] + user.mean[i]
}

#4) You can see that our computation agrees with the one returned by recommenderlab
cbind(myout[1:20, 1:3], mypred[1:20, 1:3])