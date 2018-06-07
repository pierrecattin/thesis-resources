#' Find min.prob threshold maximizing dev SR
#'
#' @param y.train : factor with levels -1, 1
#' @param y.dev  : factor with levels -1, 1
#' @param train.probs  : matrix with 2 columns: first is probability of up, second is down
#' @param dev.probs : matrix with 2 columns: first is probability of up, second is down
#'
#' @return Named numeric containing the results of the best value for min.prop: c(min.prob, train.accuracy, train.freq, train.sr, dev.accuracy, dev.freq, dev.sr
#' @export
#'
best.min.prob <- function(y.train, y.dev, train.probs, dev.probs){
  stopifnot(ncol(train.probs)==2 & ncol(dev.probs)==2)
  stopifnot(length(y.train)==nrow(train.probs) & length(y.dev)==nrow(dev.probs))
  stopifnot(all.equal(levels(y.train), levels(y.dev), c(-1,1)))

  min.prob.seq <- seq(0.5, 1, 0.01)
  max.sr <- -99
  for(min.prob in min.prob.seq){
    dev.preds <- 1*((dev.probs[,1]>min.prob) - (dev.probs[,2]>min.prob))
    buy <- which(dev.preds==1)
    sell <- which(dev.preds==-1)

    if(length(sell)+length(buy) >0){ # Check that number of predictions > 0
      dev.accuracy <- (sum(y.dev[sell]==-1)+sum(y.dev[buy]==1))/(length(sell)+length(buy))
      dev.freq <- (length(buy)+length(sell))/length(dev.preds)
      dev.sr <- freq.adjusted.accuracy(p=dev.accuracy, q=dev.freq, m=(7.5-2)*60, n)
    } else { # if no predictions are made, break the loop
      #cat("Zero predictions made with min.prob =", min.prob, "\n")
      break
    }
    if(dev.sr>max.sr){ # if the parameter is the best so far, save the results
      #cat("New best min.prob:", min.prob, "with SR=", dev.sr, "\n")
      max.sr <- dev.sr
      best.results <- c(min.prob=min.prob,
                        train.accuracy=NA, train.freq=NA, train.sr=NA,
                        dev.accuracy=dev.accuracy, dev.freq=dev.freq, dev.sr=dev.sr)
    }
  }
  # Evaluate model on training sample
  train.preds <- 1*((train.probs[,1]>best.results["min.prob"]) - (train.probs[,2]>best.results["min.prob"]))
  buy <- which(train.preds==1)
  sell <- which(train.preds==-1)
  hold <- which(train.preds==0)
  train.accuracy <- (sum(y.train[sell]==-1)+sum(y.train[buy]==1))/(length(sell)+length(buy))
  train.freq <- (length(buy)+length(sell))/length(train.preds)
  train.sr <- freq.adjusted.accuracy(p=train.accuracy, q=train.freq, m=(7.5-2)*60, n)
  best.results[c("train.accuracy", "train.freq", "train.sr")] <- c(train.accuracy, train.freq, train.sr)
  return(best.results)
}
