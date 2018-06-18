#' Deprecated: Find min.prob threshold maximizing dev SR
#'
#' @param y.dev  : factor with levels -1, 1
#' @param dev.probs : matrix with 2 columns: first is probability of up, second is down
#'
#' @return Named numeric containing the results of the best value for min.prop: c(min.prob, dev.accuracy, dev.freq, dev.sr)
#'
best.min.prob <- function(y.dev, dev.probs){
  stopifnot(ncol(dev.probs)==2)
  stopifnot(length(y.dev)==nrow(dev.probs))
  stopifnot(all.equal(levels(y.dev), c("-1","1")))

  min.prob.seq <- seq(0.5, 1, 0.01)
  max.sr <- -99
  for(min.prob in min.prob.seq){
    results <- evaluate.predictions(y.dev, dev.probs, min.prob)
    if(results[1]=="no predictions") { # if no predictions are made, break the loop
      break
    }
    if(results["sr"]>max.sr){ # if the parameter is the best so far, save the results
      max.sr <- results["sr"]
      best.results <- results
    }
  }
  names(best.results)[2:4] <- paste0("dev.", names(best.results)[2:4])
  return(best.results)
}
