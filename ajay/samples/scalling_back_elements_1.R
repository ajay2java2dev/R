?attr
# manual calculation
x <- 1:10
mean (x)
sd(x)
x.mean = sum(x) / length(x)
x.manual.scale = (x - mean(x)) / sd (x)
x.manual.scale.t = t(t(x.manual.scale))
mean (x.manual.scale.t) # note the mean of residuals is 0. The contraint

# auto scalling
x.auto.scale = scale(x, center = TRUE, scale = TRUE)
attributes(x.auto.scale)
attr(Xs, 'scaled:center') # MEAN
attr(Xs, 'scaled:scale') # SD
x.auto.scale
mean (x.auto.scale) # residual mean would be 0. The contraint

#x.manual.scale.t %/% x.auto.scale # same

## MATRIX SCALLING
rand.matrix = matrix (c(1:20), ncol=5)
for (j in 1:dim (rand.matrix)[2]) { 
  colMean[j] = mean(rand.matrix[, j])
  colSd[j] = sd (rand.matrix[, j])
  for (i in 1:dim (rand.matrix)[1]) {
    rand.matrix[i,j] = (rand.matrix[i,j] - colMean[j]) / colSd[j]
  }
}
attr(rand.matrix,"center")=colMean
attr(rand.matrix,"scale")=colSd
rand.matrix
attributes(rand.matrix)

rand.matrix = matrix (c(1:20), ncol=5) # reset the rand.matrix
rand.matrix.auto.scale = scale(rand.matrix,center = TRUE, scale = TRUE)
rand.matrix.auto.scale

#TODO: Try this in implementation
rand.matrix.auto.unscale = t(apply(rand.matrix.auto.scale, 1,  
                                   function(r) r * attr(rand.matrix.auto.scale, 'scaled:scale') +  
                                     attr(rand.matrix.auto.scale, 'scaled:center'))) 

rand.matrix.auto.unscale



#rand.matrix.man = rand.matrix - mean (rand.matrix) / sd(rand.matrix) # this is wrong
#FIXME: or this using sweep function
standardize = function (rand.matrix) {
  x1 = sweep(rand.matrix, 2, colMeans(rand.matrix),"-")
  x2 = sweep(x1, 2, sd(rand.matrix),"/")
  x2
}
standardize(rand.matrix = rand.matrix)


sweep(rand.matrix, 2, colMeans(rand.matrix),"-")
sweep(rand.matrix, 2, sd(rand.matrix),"/")

rand.matrix.elements = apply(rand.matrix, 2, function (x) (x - colMeans(rand.matrix) / sd(x)))
rand.matrix.elements = apply(rand.matrix, 2, function (x) (x - colMeans(rand.matrix) / sd(x)))
