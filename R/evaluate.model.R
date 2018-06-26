#' DEPRECATED: Compute performance metrics over all min probs
#'
#' @param y  numeric/factor, 1 if up, -1 if down
#' @param probs numeric, probability of up
#' @param n number of stocks
#'
#' @return Named numeric containing the results of the best value for min.prop, as well as the mean sr across min probs:
#'  c(min.prob, accuracy, freq, sr, mean.sr)
#'
evaluate.model <- function(y, probs, n){
  warning("evaluate.model is deprecated")
  stopifnot(length(y)==length(probs))

  min.prob.seq <- seq(0.5, 1, 0.01)
  sr <- rep(NA, length(min.prob.seq))
  accuracy <- rep(NA, length(min.prob.seq))
  max.sr <- -99
  for(min.prob in min.prob.seq){
    iter.metrics <- evaluate.predictions(y, probs, min.prob, n)
    if(iter.metrics[1]=="no predictions") { # if no predictions are made, break the loop
      break
    }
    sr[which(min.prob.seq==min.prob)] <- iter.metrics["sr"]
    accuracy[which(min.prob.seq==min.prob)] <- iter.metrics["accuracy"]
    if(iter.metrics["sr"]>max.sr){ # if the parameter is the best so far, save the results
      max.sr <- iter.metrics["sr"]
      performance <- iter.metrics
    }
  }
  performance <- c(performance, mean.sr=mean(sr, na.rm=T), mean.accuracy=mean(accuracy, na.rm=T))
  return(performance)
}
