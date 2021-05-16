
library(mclust)
head(faithful)

n <- 10000
p_true <- 0.85 # prob of using first coin
a_true <-  0.50 # the first coin has P(heads) = 0.50
b_true <-  0.70 # the second coin has P(heads) = 0.70
true <- c(p_true,a_true,b_true)
u <- ifelse(runif(n)<p_true, rbinom(n,1,a_true),rbinom(n,1,b_true))

# Set parameter estimates
p_init = 0.70; a_init = 0.70; b_init = 0.60

sample = u
head(sample)

p = p_init
a = a_init
b = b_init

p_expectation <- (p*dbinom(sample,1,a)) / ( p*dbinom(sample,1,a) + (1-p)*dbinom(sample,1,b) )

dbinom(as.matrix(faithful[,2]), 1, 0.5)
warnings()