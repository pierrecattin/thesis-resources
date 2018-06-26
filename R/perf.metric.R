#' Performance Metric
#'
#' @param freq.acc data frame containing at least columns Freqency, Accuracy and Sr if metric is "mean.sr"
#' @param metric c("mean.accuracy", "mean.sr")
#'
#' @return Performance metric computed as a mean accros frequency levels
#' @export
#'
perf.metric <- function(freq.acc, metric){
  freq.grid <- seq(min(round(freq.acc$Frequency, 2)),
                   max(round(freq.acc$Frequency, 2)), by=0.01)

  # find lines in freq.acc that have the closest freqency to each element of freq.grid
  find.closest <- function(freq){
    return(which.min(abs(freq.acc$Frequency - freq)))
  }
  indices <- sapply(X=freq.grid, FUN=find.closest)

  # compute metric
  freq.acc.filtered <- freq.acc[indices,]
  value <- switch(metric,
         mean.accuracy = mean(freq.acc.filtered$Accuracy),
         mean.sr = mean(freq.acc.filtered$Sr),
         stop("unknown metric"))
  return(value)
  }
