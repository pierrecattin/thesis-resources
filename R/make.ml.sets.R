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
#'
#' @return list containing x.train, x.dev, (x.test), y.train, y.dev, (y.test)
#' @export
#'
make.ml.sets <- function(x, y, training.start, training.end, dev.end, test.end){
  # only keep full observations for y and indicators:
  # remove first observations each day because indicators cannot be computed,
  # and last observations because y cannot be computed
  if(nrow(x)!=length(y))
    stop("x and y don't have the same length")
  if (sum(time(y) != time(x))!=0){
    stop("x and y don't have the same time index")
  }
  full.obs <- (rowSums(is.na(x))==0) & (!is.na(y))
  y.full <- y[full.obs]
  x.full <- x[full.obs, ]

  # split into training, dev and test
  stopifnot(training.end>training.start)
  stopifnot(dev.end>training.end)

  training.index <- time(x.full)>=training.start & time(x.full) <= training.end+1
  y.full<- factor(y.full$movement)
  x.train <- x.full[training.index,]
  y.train <- y.full[training.index]

  dev.index <- time(x.full)>training.end+1 & time(x.full)<=dev.end+1
  x.dev <- x.full[dev.index,]
  y.dev <- y.full[dev.index]
  cat("training set has ", nrow(x.train), " observations \n")
  cat("dev set has ", nrow(x.dev), " observations \n")

  if(missing(test.end)){
    return(list(x.train=x.train, x.dev=x.dev, y.train=y.train, y.dev=y.dev))
  } else{
    stopifnot(test.end>dev.end)
    test.index <- time(x.full)>dev.end+1 & time(x.full)<=test.end+1
    x.test <- x.full[test.index,]
    y.test <- y.full[test.index]
    cat("test set has ", nrow(x.test), " observations \n")
    return(list(x.train=x.train, x.dev=x.dev, x.test=x.test, y.train=y.train, y.dev=y.dev, y.test=y.test))
  }
}
