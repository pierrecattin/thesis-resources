#' Get accuracy and frequency for all minimum supports
#'
#' @param y numeric -1, +1
#' @param probs numeric indicating probability of up
#' @param granularity numeric indicating granularity for the sequence of min support
#' @param set optional character indicating the name of the set
#' @param Horizon optional numeric indicating the horizon
#' @param conf optional numeric inidcating the confidence level to compute confidence bounds
#' @param min.preds numeric indicating the minimum number of predictions for a frequency level to be considered
#' @param sr logical indicating if sr should be returned
#'
#' @return dataframe with columns min.prob, Accuracy, Frequency, Set if specified, Horizon if specified, lower and upper if conf is specified
#' @export
#'
get.acc.freq <- function(y, probs, granularity, set, horizon, conf, min.preds=0, sr=F, n=7){
  min.prob.seq <- seq(0.5, 1, granularity)
  data <- data.frame(min.prob=min.prob.seq, Accuracy=rep(NA, length(min.prob.seq)), Frequency=rep(NA, length(min.prob.seq)))

  for (min.prob in min.prob.seq){
    results <- evaluate.predictions(y, probs, min.prob, n)
    if(results[1]=="no predictions") { # if no predictions are made, break the loop
      break
    }
    data[which(min.prob.seq==min.prob), c("Accuracy", "Frequency")] <- results[c("accuracy", "freq")]
    if(sr){
      data[which(min.prob.seq==min.prob),"Sr"] <- results["sr"]
    }
  }
  data <- data[!is.na(data$Accuracy), ]

  n.small <- length(probs) * data$Frequency  < min.preds
  data <- data[!n.small, ] # remove datapoints with little observations

  if(!missing(set))
    data$Set <- set

  if(!missing(horizon))
    data$Horizon <- horizon

  if(!missing(conf)){
    bounds <- confidence.bound(p=data$Accuracy, n=length(probs)*data$Frequency, conf)
    bounds[bounds[,1]<0,1] <- 0
    bounds[bounds[,2]>1,2] <- 1
    data <- cbind(data, bounds)
  }
  return(data)
}
