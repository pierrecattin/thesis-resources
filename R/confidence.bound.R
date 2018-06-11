#' Confidence Bound
#'
#' @param p Sample proportion; numeric
#' @param n Number of observations; numeric
#' @param conf: confidence level within the bounds
#'
#' @return dataframe with values of both bounds of each element
#' @export
#'
#' @examples confidence.bound(c(0.7, 0.8), c(100000, 500), 0.95)
confidence.bound <- function(p, n, conf=0.95){
  stopifnot(length(p)==length(n))
  distance <- -sqrt(p*(1-p)/n)*qnorm((1-conf)/2)
  bounds <- data.frame(lower=p-distance, upper=p+distance)
  return(bounds)
}
confidence.bound(c(0.7, 0.8), c(1000, 500), 0.95)
