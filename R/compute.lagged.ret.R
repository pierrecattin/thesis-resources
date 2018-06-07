#' Compute lagged returns
#' Compute lagged return each day up to a specified lag
#'
#' @param prices OLHCV prices series; xts
#'
#' @return xts with one column per lag. Index is same as index of prices.
#' @export
#'
compute.lagged.ret <- function(prices, max.lag=120){

  all.days <- as.Date(time(prices))
  days <- unique(all.days)

  lagged.ret <- matrix(NA, ncol=max.lag, nrow=nrow(prices))
  colnames(lagged.ret)<- paste0("lag ", 1:max.lag)
  lagged.ret <- as.xts(lagged.ret, order.by=time(prices))
  # d <- days[1]
  pb.ind <- tkProgressBar(title = "Computing lagged returns", min = 0,
                          max = length(days), width = 300)
  d <- days[1]
  for (d in days){
    day.num <- which(days==d) # only for pb
    setTkProgressBar(pb.ind, day.num, label=paste0(day.num, "/", length(days), " days computed"))
    closes <- as.numeric(prices[all.days==d,"close"])

    apply.fun <- function(n){
      return(ROC(closes, n=n, type="discrete"))
    }
    lagged.ret[all.days==d, ] <- sapply(X=1:max.lag, FUN=apply.fun)
  }
  close(pb.ind)
  return(lagged.ret)
}
