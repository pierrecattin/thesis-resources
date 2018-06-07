#' Confidence Bound
#'
#' @param p Sample proportion; numeric
#' @param n Number of observations; numeric
#' @param alpha Unilateral probability of being outside of the bound; numeric
#' @param lower Lower or upper bound; logi
#'
#' @return value of the bound (numeric)
#' @export
#'
#' @examples confidence.bound(0.65, 110, 0.05, T)
confidence.bound <- function(p, n, alpha=0.05, lower=T){
  # p : achieved accuracy
  # n: numer of trades
  # alpha: unilateral prob of being outside
  # lower:
  return(p-(sqrt(p*(1-p)/n)*qnorm(alpha))*(1-2*lower)) # (1-alpha)*100 % (unilateral) bound on true accuracy
}
