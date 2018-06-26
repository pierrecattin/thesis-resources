#' Evaluate predictions performance against true label
#'
#' @param y : numeric/factor, 1 if up, -1 if down
#' @param probs : numeric probability of up
#' @param min.prob: numeric indicating minimum confidence required to make a prediction
#' @param check: logical indicating if dimension checks should be performed
#' @param n number of stocks (only relevant if sr is needed)
#'
#' @return Named numeric containing c(min.prob, accuracy, freq, sr), or "no predictions" if min.prob is too high
#' @export
#'
evaluate.predictions <- function(y, probs, min.prob, n=7, check=F){
  if(check){
    stopifnot(length(y)==length(probs))
  }

  preds <- 1*(probs>=min.prob) - 1*(probs<(1-min.prob))
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
