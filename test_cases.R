rm(list=ls())
library(ggplot2)
library(grid)
library(tensorflow)
library(keras)
source("R/evaluate.model.R")
source("R/evaluate.predictions.R")
source("R/freq.adjusted.accuracy.R")
source("R/confidence.bound.R")
source("R/freq.vs.accuracy.plot.R")
source("R/freq.vs.accuracy.multiplot.R")
source("R/multiplot.R")
source("R/metric.exp.sr.R")
source("R/metric.mean.accuracy.R")
source("R/train.nn.R")

#### Evaluate model and preds ####
L <- 10000
probs <- runif(L)
y <- 1*((probs+rnorm(L)/3)>0.5)*2-1
n <- 8

evaluate.model(y, probs, n)
evaluate.predictions(y, probs, 0.71, n, T)

#### multiplot ####
L <- 10000
train.probs <- runif(L)
y.train <- 1*((train.probs+rnorm(L)/5)>0.5)*2-1
dev.probs <- runif(L, 0.2, 0.8)
y.dev <- 1*((dev.probs+rnorm(L)/4)>0.5)*2-1
test.probs <- runif(L, 0.4, 0.6)
y.test <- 1*((test.probs+rnorm(L)/4)>0.5)*2-1
conf <- 0.95; stock<-"AAPL"; horizon<- 60
freq.vs.accuracy.multiplot(y.train, train.probs,
                               y.dev, dev.probs,
                               y.test, test.probs,
                               conf, stock, horizon)







#### TF metrics ####
# create data
L <- 2000
probs <- runif(L)
y <- 1*((probs+rnorm(L)*2)>0.5)
probs <- matrix(probs, ncol=1)
y <- matrix(y, ncol=1)


# convert as tensors
y_pred <- tf$constant(probs, dtype="float32")
y_true <- tf$constant(y, dtype="int32")

# evaluate functions
sess <- tf$Session()
sess$run(metric.exp.sr(y_true, y_pred))
sess$run(metric.mean.accuracy(y_true, y_pred))
sess$close()

# check
evaluate.model(y*2-1, probs, n)


#### train.nn ####
# generate data
N <- 3000
x <- matrix(rnorm(N*10), ncol=10)
y <- ((colSums(x)+rnorm(N)*10)>0)*2-1
y <- as.factor(y)
x.train <- x[1:(N/2),]; x.dev <- x[(N/2+1):N,]
y.train <- y[1:(N/2)]; y.dev <- y[(N/2+1):N]

# define model
batch.size <- 128
epochs <- 100
structure <- c(5)
activations <- "relu"
l2 <- 0.005
learning.rate <- 0.002


fit <- train.nn(x.train, x.dev, y.train, y.dev,
                     epochs, batch.size, activations, structure,
                     l2, learning.rate, perf.metric="mean.accuracy")
