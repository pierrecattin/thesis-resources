#' Evaluate predictions performance against true label
#'
#' @param y  : factor with levels -1, 1
#' @param probs : matrix with 2 columns: first is probability of up, second is down
#' @param min.prob: numeric indicating minimum confidence required to make a prediction
#' @param check: logical indicating if dimension checks should be performed
#'
#' @return Named numeric containing c(min.prob, accuracy, freq, sr), or "no predictions" if min.prob is too high
#' @export
#'
evaluate.predictions <- function(y, probs, min.prob, check=F){
  if(check){
    stopifnot(ncol(probs)==2)
    stopifnot(length(y)==nrow(probs))
    stopifnot(all.equal(levels(y), c("-1","1")))
  }

  preds <- 1*((probs[,1]>min.prob) - (probs[,2]>min.prob))
  if(sum(preds!=0)>0){ # Check that number of predictions > 0
    buy <- which(preds==1)
    sell <- which(preds==-1)
    accuracy <- (sum(y[sell]==-1)+sum(y[buy]==1))/(length(sell)+length(buy))
    freq <- (length(buy)+length(sell))/length(preds)
    sr <- freq.adjusted.accuracy(p=accuracy, q=freq, m=(7.5-2)*60, n)
    return(c(min.prob=min.prob, accuracy=accuracy, freq=freq, sr=sr))
  } else{
    return("no predictions")
  }
}
