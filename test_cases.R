source("R/evaluate.model.R")
source("R/evaluate.predictions.R")
source("R/freq.adjusted.accuracy.R")

L <- 1000
probs <- runif(L)
y <- 1*((probs+rnorm(L)/3)>0.5)
n <- 8

evaluate.model(y, probs, n)
evaluate.predictions(y, probs, 0.71, n, T)
