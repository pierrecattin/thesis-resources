source("R/freq.adjusted.accuracy.R")
source("R/best.min.prob.mean.R")
source("R/evaluate.predictions.num.R")

n <- 8
L <- 10000
probs <- runif(L)
y <- 1*((probs)>0.5+rnorm(L)/3)*2-1
hist(probs[y==1]);hist(probs[y==-1])

best.min.prob.mean(y, probs, n)
evaluate.predictions.num(y, probs, 0.5, n, T)
