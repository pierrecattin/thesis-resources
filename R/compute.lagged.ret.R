#' Compute lagged returns
#' Compute lagged return each day up to a specified lag
#'
#' @param prices OLHCV prices series; xts
#' @param max.lag indicates how far back lagged returns should be computed
#' @param loading logical telling if loading bar should be displayed
#' @param volume logical indicating if volume should be added
#'
#' @return xts with one column per lag. Index is same as index of prices.
#' @export
#'
compute.lagged.ret <- function(prices, max.lag=120, loading=T, volume=T){

  all.days <- as.Date(time(prices))
  days <- unique(all.days)

  lagged.ret <- matrix(NA, ncol=max.lag, nrow=nrow(prices))
  colnames(lagged.ret)<- paste0("ret.lag.", 1:max.lag)
  lagged.ret <- as.xts(lagged.ret, order.by=time(prices))
  lagged.vol <- lagged.ret
  colnames(lagged.vol)<- paste0("vol.lag.", 1:max.lag)

  # apply.lagged.ret <- function(n){
  #   lagged.closes <- closes[1:(length(closes)-n)]
  #   lagged.closes <- c(rep(NA, n), lagged.closes)
  #   return(closes/lagged.closes-1)
  # }

  apply.lagged.ret <- function(n){
    lagged.closes <- c(NA, closes[1:(length(closes)-1)])
    lagged.ret <- closes/lagged.closes-1 # one-minute returns
    lagged.ret <- lagged.ret[1:(length(lagged.ret)-n+1)] # n-1 lags one-minute returns
    lagged.ret <- c(rep(NA, n-1), lagged.ret)
    return(lagged.ret)
  }

  apply.lagged.vol <- function(n){
    lagged.vol <- volumes[1:(length(volumes)-n)]
    lagged.vol <- c(rep(NA, n), lagged.vol)
    return(lagged.vol)
  }
  if (loading)
    pb.ind <- tkProgressBar(title = "Computing lagged returns", min = 0,
                            max = length(days), width = 300)
  #d <- days[1]
  for (d in days){
    if (loading) {
      day.num <- which(days==d) # only for pb
      setTkProgressBar(pb.ind, day.num, label=paste0(day.num, "/", length(days), " days computed"))
    }
    closes <- as.numeric(prices[all.days==d,"close"])

    lagged.ret[all.days==d, ] <- sapply(X=1:max.lag, FUN=apply.lagged.ret)

    if(volume){
      volumes <- as.numeric(prices[all.days==d,"volume"])
      lagged.vol[all.days==d, ] <- sapply(X=1:max.lag, FUN=apply.lagged.vol)
    }
  }
  if (loading)
    close(pb.ind)

  if(volume){
    features <- cbind(lagged.ret, lagged.vol)
  } else{
    features <- lagged.ret
  }
  return(features)
}

