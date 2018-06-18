#' Compute performance metrics over all min probs
#'
#' @param y  numeric/factor, 1 if up, -1 if down
#' @param probs numeric, probability of up
#' @param n number of stocks
#'
#' @return Named numeric containing the results of the best value for min.prop, as well as the mean sr across min probs: c(min.prob, accuracy, freq, sr, mean.sr)
#' @export
#'
evaluate.model <- function(y, probs, n){
  stopifnot(length(y)==length(probs))

  min.prob.seq <- seq(0.5, 1, 0.01)
  sr <- rep(NA, length(min.prob.seq))
  max.sr <- -99
  for(min.prob in min.prob.seq){
    results <- evaluate.predictions(y, probs, min.prob, n)
    if(results[1]=="no predictions") { # if no predictions are made, break the loop
      break
    }
    sr[which(min.prob.seq==min.prob)] <- results["sr"]
    if(results["sr"]>max.sr){ # if the parameter is the best so far, save the results
      max.sr <- results["sr"]
      best.results <- results
    }
  }
  best.results <- c(best.results, mean.sr=mean(sr, na.rm=T))
  return(best.results)
}