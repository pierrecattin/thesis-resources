library(ggplot2)
source("R/evaluate.model.R")
source("R/evaluate.predictions.R")
source("R/freq.adjusted.accuracy.R")
source("R/confidence.bound.R")

L <- 10000
probs <- runif(L)
y <- 1*((probs+rnorm(L)/3)>0.5)*2-1
n <- 8

evaluate.model(y, probs, n)
evaluate.predictions(y, probs, 0.71, n, T)

## multiplot
library(grid)
source("R/freq.vs.accuracy.plot.R")
source("R/freq.vs.accuracy.multiplot.R")
L <- 10000
train.probs <- runif(L)
y.train <- 1*((train.probs+rnorm(L)/5)>0.5)*2-1
dev.probs <- runif(L)
y.dev <- 1*((dev.probs+rnorm(L)/4)>0.5)*2-1
test.probs <- runif(L)
y.test <- 1*((test.probs+rnorm(L)/4)>0.5)*2-1
conf <- 0.95; stock<-"AAPL"; horizon<- 60
freq.vs.accuracy.multiplot(y.train, train.probs,
                               y.dev, dev.probs,
                               y.test, test.probs,
                               conf, stock, horizon)
