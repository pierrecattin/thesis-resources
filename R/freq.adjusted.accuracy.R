#' Frequency adjusted accuracy
#'
#' @param p: accuracy
#' @param q: frequency of prediction other than hold
#' @param m: Average number of predictions made per day
#' @param n: Number of stocks
#'
#' @return: Frequency adjusted accuracy (scalar or vector)
#' @export
#'
freq.adjusted.accuracy <- function(p, q, m, n){
  #return(((2*p*q*m*n - q*m*n)) / sqrt(m*n*q*(-4*p^2*q-q+1)))
  return(sqrt(q*m*n)*(2*p-1)) / sqrt(-4*p^2*q-q+1)
}
