#' Make training/dev/test test
#'
#' Removes incomplete observations and split the time series into distinct sets.
#'
#' @param x : xts of features
#' @param y : xts of true classes
#' @param training.start : date
#' @param training.end : date
#' @param dev.end : date
#' @param test.end : date. Optional, if missing, no test set is created
#' @param test.train.start : date. Optional. Start of the period used to train the model for the test set
#' @param scale: inidcates if x should be scaled to mean 0 and sd 1 (based on training sample mean and sd)
#'
#' @return list containing x.train(xts), x.dev(xts), (x.test(xts)), y.train(factor), y.dev(factor), (y.test(factor))
#' @export
#'
make.ml.sets <- function(x, y, training.start, training.end, dev.end, test.end, train.for.test.start, scale=T){
  # only keep full observations for y and indicators:
  # remove first observations each day because indicators cannot be computed,
  # and last observations because y cannot be computed
  if(nrow(x)!=length(y))
    stop("x and y don't have the same length")
  if (sum(time(y) != time(x))!=0){
    stop("x and y don't have the same time index")
  }
  full.obs <- (rowSums(is.na(x))==0) & (!is.na(y))
  x.full <- x[full.obs, ]

  y.full <- y[full.obs]
  stopifnot(all.equal(time(y.full), time(x.full)))
  y.full <- factor(y.full$movement)

  # split into training, dev and test
  stopifnot(training.end>training.start)
  stopifnot(dev.end>training.end)

  days <- as.Date(time(x.full))


  training.index <- days>=training.start & days <= training.end
  dev.index <- days>training.end & days <= dev.end

  x.train <- x.full[training.index,]
  y.train <- y.full[training.index]
  x.dev <- x.full[dev.index,]
  y.dev <- y.full[dev.index]
  # min(time(x.train)); max(time(x.train))
  # min(time(x.dev)); max(time(x.dev))

  if(scale){
    x.means <- matrix(colMeans(x.train), nrow=1)
    x.sd <- matrix(apply(x.train,2, sd), nrow=1)
    #dim(x.train); dim(x.means); dim(x.sd)
    x.train <- (x.train-matrix(rep(x.means, nrow(x.train)), nrow=nrow(x.train), byrow=T))/matrix(rep(x.sd, nrow(x.train)), nrow=nrow(x.train), byrow=T)
    x.dev <- (x.dev-matrix(rep(x.means, nrow(x.dev)), nrow=nrow(x.dev), byrow=T))/matrix(rep(x.sd, nrow(x.dev)), nrow=nrow(x.dev), byrow=T)
  }


  cat("training set has ", nrow(x.train), " observations \n")
  cat("dev set has ", nrow(x.dev), " observations \n")

  if(missing(test.end)){
    return(list(x.train=x.train, x.dev=x.dev, y.train=y.train, y.dev=y.dev))
  } else{
    stopifnot(test.end>dev.end)
    test.index <- days > dev.end & days <= test.end
    x.test <- x.full[test.index,]
    y.test <- y.full[test.index]
    if(scale){
      x.test <- (x.test-matrix(rep(x.means, nrow(x.test)), nrow=nrow(x.test), byrow=T))/matrix(rep(x.sd, nrow(x.test)), nrow=nrow(x.test), byrow=T)
    }
    cat("test set has ", nrow(x.test), " observations \n")
    #min(time(x.test)); max(time(x.test))
    if(missing(train.for.test.start)){
      return(list(x.train=x.train, x.dev=x.dev, x.test=x.test, y.train=y.train, y.dev=y.dev, y.test=y.test))
    } else{
      train.for.test.index <- days >= train.for.test.start & days <= dev.end
      x.train.for.test <- x.full[train.for.test.index,]
      y.train.for.test <- y.full[train.for.test.index]
      if(scale){
        x.train.for.test <- (x.train.for.test-matrix(rep(x.means, nrow(x.train.for.test)), nrow=nrow(x.train.for.test), byrow=T))/
          matrix(rep(x.sd, nrow(x.train.for.test)), nrow=nrow(x.train.for.test), byrow=T)
      }
      cat("train for test set has ", nrow(x.train.for.test), " observations \n")
      return(list(x.train=x.train, x.dev=x.dev, x.test=x.test, y.train=y.train, y.dev=y.dev, y.test=y.test, x.train.for.test=x.train.for.test, y.train.for.test=y.train.for.test))
    }
  }
}

# x <- indicators
# test.end <- testing.end
# y <- y[[1]]
