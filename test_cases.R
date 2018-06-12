source("R/freq.adjusted.accuracy.R")
source("R/best.min.prob.mean.R")
source("R/evaluate.predictions.num.R")


L <- 1000
probs <- runif(L)
y <- 1*((probs+rnorm(L)/3)>0.5)
n <- 8

best.min.prob.mean(y, probs, n)
evaluate.predictions.num(y, probs, 0.71, n, T)
