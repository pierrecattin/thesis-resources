rm(list=ls()); gc()
setwd("C:/Users/catti/Desktop/thesis_data_analysis/thesis.resources")
library(stats)
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
source("R/perf.metric.R")
source("R/freq.vs.accuracy.plot.R")
source("R/freq.vs.accuracy.multiplot.R")
source("R/freqacc.byset.byhoriz.plot.R")
source("R/metric.exp.sr.R")
source("R/metric.mean.accuracy.R")
source("R/train.nn.R")
source("R/compute.indicators.R")
source("R/make.ml.sets.R")
source("R/combine.forecasts.R")


#### combine.forecasts ####
setwd("C:/Users/catti/Desktop/thesis_data_analysis/thesis.main")
lr <- readRDS("saved_results/all_stocks/logit_all_taFALSE.RDS")
knn <- readRDS("saved_results/all_stocks/knn_all.RDS")
rf <- readRDS("saved_results/all_stocks/rf_all.RDS")
ffann <- readRDS("saved_results/all_stocks/ffann_all.RDS")
#svm <- readRDS("saved_results/all_stocks/svm_all.RDS")

stock <- "AAPL"
horizon <- 1
set <- "Validation Set"
predictions.dev <- cbind(lr=lr[[stock]]$y.probs.and.true[lr[[stock]]$y.probs.and.true$horizon == horizon & lr[[stock]]$y.probs.and.true$set == set, ]$prob,
                    knn=knn[[stock]]$y.probs.and.true[lr[[stock]]$y.probs.and.true$horizon == horizon & lr[[stock]]$y.probs.and.true$set == set, ]$prob,
                    rf=rf[[stock]]$y.probs.and.true[lr[[stock]]$y.probs.and.true$horizon == horizon & lr[[stock]]$y.probs.and.true$set == set, ]$prob,
                    ffann=ffann[[stock]]$y.probs.and.true[lr[[stock]]$y.probs.and.true$horizon == horizon & lr[[stock]]$y.probs.and.true$set == set, ]$prob)
head(predictions.dev)
y.dev <- lr[[stock]]$y.probs.and.true[lr[[stock]]$y.probs.and.true$horizon == horizon & lr[[stock]]$y.probs.and.true$set == set, ]$y.true
summary(y.dev)

set <- "Testing Set"
predictions.test <- cbind(lr=lr[[stock]]$y.probs.and.true[lr[[stock]]$y.probs.and.true$horizon == horizon & lr[[stock]]$y.probs.and.true$set == set, ]$prob,
                         knn=knn[[stock]]$y.probs.and.true[lr[[stock]]$y.probs.and.true$horizon == horizon & lr[[stock]]$y.probs.and.true$set == set, ]$prob,
                         rf=rf[[stock]]$y.probs.and.true[lr[[stock]]$y.probs.and.true$horizon == horizon & lr[[stock]]$y.probs.and.true$set == set, ]$prob,
                         ffann=ffann[[stock]]$y.probs.and.true[lr[[stock]]$y.probs.and.true$horizon == horizon & lr[[stock]]$y.probs.and.true$set == set, ]$prob)
head(predictions.test)
y.test <- lr[[stock]]$y.probs.and.true[lr[[stock]]$y.probs.and.true$horizon == horizon & lr[[stock]]$y.probs.and.true$set == set, ]$y.true
summary(y.test)

f <- combine.forecasts(predictions.dev, predictions.test, y.dev)

#### Sanity checks ####
setwd("C:/Users/catti/Desktop/thesis_data_analysis/thesis.main")
lr <- readRDS("saved_results/all_stocks/logit_all.RDS")
knn <- readRDS("saved_results/all_stocks/knn_all.RDS")
rf <- readRDS("saved_results/all_stocks/rf_all.RDS")
ffann <- readRDS("saved_results/all_stocks/ffann_all.RDS")
svm <- readRDS("saved_results/all_stocks/svm_all.RDS")

for(stock in names(ffann)){
  print("##################");print(stock)
  ffnn.sub <- ffann[[stock]]$y.probs.and.true$y.true
  knn.sub <- knn[[stock]]$y.probs.and.true$y.true
  rf.sub <- rf[[stock]]$y.probs.and.true$y.true
  svm.sub <- svm[[stock]]$y.probs.and.true$y.true
  stopifnot(all.equal(ffnn.sub, knn.sub, rf.sub, svm.sub))
}

stock <- "XOM"
for(stock in names(ffann)){
  print("##################");print(stock)
  ffnn.sub <- ffann[[stock]]$y.probs.and.true
  lr.sub <- lr[[stock]]$y.probs.and.true
  # knn <- knn[[stock]]$y.probs.and.true
  # rf <- rf[[stock]]$y.probs.and.true
  print("Training")
  cat("ref", sum(ffnn.sub$set=="Training Set"), "\n")
  cat("lr", sum(lr.sub$set=="Training Set"), "\n")
  print("Validation")
  cat("ref", sum(ffnn.sub$set=="Validation Set"), "\n")
  cat("lr", sum(lr.sub$set=="Validation Set"), "\n")
  print("Testing")
  cat("ref", sum(ffnn.sub$set=="Testing Set"), "\n")
  cat("lr", sum(lr.sub$set=="Testing Set"), "\n")
}
# AAPL
# ffnn    logit   knn     rf
# 986832  986832  986832  986832
# XOM
# 985844  986832  985844  985844
# MSFT
# 986263  986832  986263  986263


model <- "knn"
#res <- readRDS(paste0("saved_results/all_stocks/logit_all_taTRUE.RDS"))
res <- readRDS(paste0("saved_results/all_stocks/", model, "_all.RDS"))
stock <- names(res)[4]
for(stock in names(res)){
    y.true <- ffann[[stock]]$y.probs.and.true$y.true
    res[[stock]]$y.probs.and.true$horizon <- as.numeric(res[[stock]]$y.probs.and.true$horizon)
    res[[stock]]$y.probs.and.true$prob <- as.numeric(res[[stock]]$y.probs.and.true$prob)
    res[[stock]]$y.probs.and.true$y.true <- y.true
    gc()
}
saveRDS(res, paste0("saved_results/all_stocks/", model, "_all.RDS"))






#### get.acc.freq ####
L <- 10000
probs <- runif(L)
y <- 1*((probs+rnorm(L)/5)>0.5)*2-1
granularity <- 0.001; set="t"; horizon=17; conf=0.99; min.preds=0; sr=T; n=7
freq.acc <- get.acc.freq(y, probs, granularity, set, horizon, conf, min.preds, sr, n)
# p <- freq.acc$Accuracy
# q<-freq.acc$Frequency
# m <- 7*5.5*60
# plot(x=sqrt(q*m)*(2*p-1)/sqrt(-4*p^2*q+4*p*q-q+1), y=freq.acc$Sr)
metric <- "mean.accuracy"
perf.metric(freq.acc, metric)

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
y <- readRDS("C:/Users/catti/Desktop/thesis_data_analysis/thesis.main/saved_results/y_AAPL.RDS")[[1]]
training.start <- date("2014-06-01") # start of training sample
training.end <- date("2016-05-31") # end of training sample; start of dev sample
dev.end <- date("2017-05-31")
test.end <- date("2018-05-31")
train.for.test.start <- date("2015-06-01")
scale <- T

sets <- make.ml.sets(x, y, training.start, training.end, dev.end, test.end, train.for.test.start)

for (i in 1:length(sets)){
  assign(names(sets)[i], sets[[i]])
}
rm(sets)
hist(colMeans(x.train), 100)
hist(colMeans(x.dev), 100)
hist(colMeans(x.test), 100)
hist(colMeans(x.train.for.test), 100)
head(matrix(apply(x.train,2, sd), nrow=1))
head(matrix(apply(x.dev,2, sd), nrow=1))
head(matrix(apply(x.test,2, sd), nrow=1))
head(matrix(apply(x.train.for.test,2, sd), nrow=1))

#### Evaluate model and preds ####
L <- 10000
probs <- runif(L)
y <- 1*((probs+rnorm(L)/3)>0.5)*2-1
n <- 8

evaluate.model(y, probs, n)
evaluate.predictions(y, probs, 0.71, n, T)

#### Frequency-Accuracy plots ####
# All sets and all horizons
L <- c(150000, 60000, 60000)
train.probs <- runif(L[1])
dev.probs <- runif(L[2])
test.probs <- runif(L[3])

conf <- 0.99; stock<-"AAPL"; horizon<- 60;  granularity<- 0.01; model <- "Logistic Regression"

horizon.seq <- c(1, 10, 30, 60)
freq.acc <- data.frame(min.prob=numeric(0),
                       Accuracy=numeric(0),
                       Frequency=numeric(0),
                       Set=character(0),
                       Horizon=numeric(0),
                       lower=numeric(0),
                       upper=numeric(0))

model <- "Logistic Regression"
rand.mat <- matrix(c(5, 10, 10,
                     5, 20, 20,
                     4, 25, 25,
                     3, 300, 300), nrow=4, byrow=T)

for(horizon in horizon.seq){
  line <- which(horizon.seq == horizon)
  y.train <- 1*((train.probs+rnorm(L[1])*rand.mat[line,1])>0.5)*2-1
  dev.probs <- runif(L[2], 0, 1)
  y.dev <- 1*((dev.probs+rnorm(L[2])*rand.mat[line,2])>0.5)*2-1
  test.probs <- runif(L[3], 0, 1)
  y.test <- 1*((test.probs+rnorm(L[3])*rand.mat[line,3])>0.5)*2-1

  freq.acc <- rbind(freq.acc,
                    get.acc.freq(y.train, train.probs, granularity, "Training Set", horizon, conf),
                    get.acc.freq(y.dev, dev.probs, granularity, "Validation Set", horizon, conf),
                    get.acc.freq(y.test, test.probs, granularity, "Testing Set", horizon, conf))

}
pdf(paste0("C:/Users/catti/Google Drive/Thesis/presentation/figures/freqacc_logit.pdf"),
    width=6.5, height=5.5)
print(freqacc.byset.byhoriz.plot(freq.acc, conf=0.99, stock, "Logistic Regression"))
dev.off()

freq.acc <- data.frame(min.prob=numeric(0),
                       Accuracy=numeric(0),
                       Frequency=numeric(0),
                       Set=character(0),
                       Horizon=numeric(0),
                       lower=numeric(0),
                       upper=numeric(0))
model <- "Neural Network"
L <- c(60000, 30000, 30000)
train.probs <- runif(L[1])
dev.probs <- runif(L[2])
test.probs <- runif(L[3])



rand.mat <- matrix(c(2, 2, 2.5,
                     2.5, 2.5, 3,
                     2.5, 2.5, 3,
                     3, 3, 4), nrow=4, byrow=T)

for(horizon in horizon.seq){
  line <- which(horizon.seq == horizon)
  y.train <- 1*((train.probs+rnorm(L[1])*rand.mat[line,1])>0.5)*2-1
  dev.probs <- runif(L[2], 0, 1)
  y.dev <- 1*((dev.probs+rnorm(L[2])*rand.mat[line,2])>0.5)*2-1
  test.probs <- runif(L[3], 0, 1)
  y.test <- 1*((test.probs+rnorm(L[3])*rand.mat[line,3])>0.5)*2-1

  freq.acc <- rbind(freq.acc,
                    get.acc.freq(y.train, train.probs, granularity, "Training Set", horizon, conf),
                    get.acc.freq(y.dev, dev.probs, granularity, "Validation Set", horizon, conf),
                    get.acc.freq(y.test, test.probs, granularity, "Testing Set", horizon, conf))

}
freqacc.byset.byhoriz.plot(freq.acc, conf, stock, model, support.color = F)
pdf(paste0("C:/Users/catti/Google Drive/Thesis/presentation/figures/freqacc_nn.pdf"),
    width=6.5, height=5.5)
print(freqacc.byset.byhoriz.plot(freq.acc, conf=0.99, stock, model))
dev.off()

model <- "Hetereogeneous Ensemble"
L <- c(60000, 30000, 30000)
train.probs <- runif(L[1])
dev.probs <- runif(L[2])
test.probs <- runif(L[3])



rand.mat <- matrix(c(1, 1.3, 1.5,
                     1, 1.7, 2,
                     1, 2, 2.5,
                     0.7, 2.5, 3), nrow=4, byrow=T)

for(horizon in horizon.seq){
  line <- which(horizon.seq == horizon)
  y.train <- 1*((train.probs+rnorm(L[1])*rand.mat[line,1])>0.5)*2-1
  dev.probs <- runif(L[2], 0, 1)
  y.dev <- 1*((dev.probs+rnorm(L[2])*rand.mat[line,2])>0.5)*2-1
  test.probs <- runif(L[3], 0, 1)
  y.test <- 1*((test.probs+rnorm(L[3])*rand.mat[line,3])>0.5)*2-1

  freq.acc <- rbind(freq.acc,
                    get.acc.freq(y.train, train.probs, granularity, "Training Set", horizon, conf),
                    get.acc.freq(y.dev, dev.probs, granularity, "Validation Set", horizon, conf),
                    get.acc.freq(y.test, test.probs, granularity, "Testing Set", horizon, conf))

}
freqacc.byset.byhoriz.plot(freq.acc, conf, stock, model, support.color = F)
pdf(paste0("C:/Users/catti/Google Drive/Thesis/presentation/figures/freqacc_ensemble.pdf"),
    width=6.5, height=5.5)
print(freqacc.byset.byhoriz.plot(freq.acc, conf=0.99, stock, model))
dev.off()



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
sess$run(metric.mean.accuracy(y_true, y_pred))
sess$close()

# check
freq.acc <- get.acc.freq(y*2-1, probs, granularity=0.01)
mean(freq.acc$Accuracy)
perf.metric(freq.acc, "mean.accuracy")

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
