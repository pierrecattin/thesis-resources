rm(list=ls())
library(ggplot2)
library(grid)
library(tensorflow)
library(keras)
library(xts)
library(TTR)
library(fGarch)
library(tcltk)
library(lubridate)
source("R/evaluate.model.R")
source("R/evaluate.predictions.R")
source("R/freq.adjusted.accuracy.R")
source("R/confidence.bound.R")
source("R/get.acc.freq.R")
source("R/freq.vs.accuracy.plot.R")
source("R/freq.vs.accuracy.multiplot.R")
source("R/freqacc.byset.byhoriz.plot.R")
source("R/metric.exp.sr.R")
source("R/metric.mean.accuracy.R")
source("R/train.nn.R")
source("R/compute.indicators.R")
source("R/make.ml.sets.R")
#### Indicators  ####
prices <- readRDS("C:/Users/catti/Desktop/thesis_data_analysis/thesis.main/data/clean/t_prices_AAPL.RDS")
prices <- prices[,-6]
prices <- prices[1:(390*10),]
tail(prices)
loading <-T
indicators <- compute.indicators(prices, loading)

library(corrplot)
M<-cor(indicators, use="complete.obs")
View(round(M,2))
corrplot(M, method="color", type="upper", tl.col="black", tl.srt=70) # , order="hclust"

#### make.ml.sets####
x <- readRDS("C:/Users/catti/Desktop/thesis_data_analysis/thesis.main/saved_results/lagged_ret_volTRUE_AAPL.RDS")
y <- indicators <- readRDS("C:/Users/catti/Desktop/thesis_data_analysis/thesis.main/saved_results/y_AAPL.RDS")[[1]]
training.start <- date("2014-06-01") # start of training sample
training.end <- date("2016-05-31") # end of training sample; start of dev sample
dev.end <- date("2017-05-31")
testing.end <- date(prices[nrow(prices),])
train.for.test.start <- date("2015-06-01")

sets <- make.ml.sets(x, y, training.start, training.end, dev.end, testing.end, train.for.test.start)

#### Evaluate model and preds ####
L <- 10000
probs <- runif(L)
y <- 1*((probs+rnorm(L)/3)>0.5)*2-1
n <- 8

evaluate.model(y, probs, n)
evaluate.predictions(y, probs, 0.71, n, T)

#### Frequency-Accuracy plots ####
L <- 10000
train.probs <- runif(L)
y.train <- 1*((train.probs+rnorm(L)/5)>0.5)*2-1
dev.probs <- runif(L, 0, 1)
y.dev <- 1*((dev.probs+rnorm(L)/4)>0.5)*2-1
test.probs <- runif(L, 0, 1)
y.test <- 1*((test.probs+rnorm(L)/4)>0.5)*2-1
conf <- 0.99; stock<-"AAPL"; horizon<- 60;  granularity<- 0.01

# Single curve
freq.vs.accuracy.plot(y.train, train.probs, conf, set="Training", ytitle=T, legend=T, granularity)

# Three sets
freq.vs.accuracy.multiplot(y.train, train.probs,
                               y.dev, dev.probs,
                               y.test, test.probs,
                               conf, stock, "Logit", horizon, granularity)


# All sets and all horizons
L <- 10000
conf <- 0.99; stock<-"AAPL"; horizon<- 60;  granularity<- 0.01; model <- "Logit"

horizon.seq <- c(1, 5, 30, 60)
freq.acc <- data.frame(min.prob=numeric(0),
                       Accuracy=numeric(0),
                       Frequency=numeric(0),
                       Set=character(0),
                       Horizon=numeric(0),
                       lower=numeric(0),
                       upper=numeric(0))
for(horizon in horizon.seq){
  train.probs <- runif(L)
  y.train <- 1*((train.probs+rnorm(L)*horizon/60)>0.5)*2-1
  dev.probs <- runif(L, 0, 1)
  y.dev <- 1*((dev.probs+rnorm(L)*horizon/30)>0.5)*2-1
  test.probs <- runif(L, 0, 1)
  y.test <- 1*((test.probs+rnorm(L)*horizon/10)>0.5)*2-1

  freq.acc <- rbind(freq.acc,
                    get.acc.freq(y.train, train.probs, granularity, "Training Set", horizon, conf),
                    get.acc.freq(y.dev, dev.probs, granularity, "Validation Set", horizon, conf),
                    get.acc.freq(y.test, test.probs, granularity, "Testing Set", horizon, conf))

}
freqacc.byset.byhoriz.plot(freq.acc, conf, stock, model)


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
