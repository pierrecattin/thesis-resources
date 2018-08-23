#' Compute Indicators
#' Computes technical indicators based on OLHCV prices series
#'
#' @param prices OLHCV prices series; xts
#' @param loading logical telling if loading bar should be displayed
#'
#' @return xts with one column per technical indicator. Index is same as index of prices.
#' @export
#'
compute.indicators <- function(prices, loading=T){

  all.days <- as.Date(time(prices))
  days <- unique(all.days)

  indicators.cols <- c("return.1", "return.10", "return.30", "return.60", "return.120",
                       "rsi.10", "rsi.60", "rsi.120",
                       "macd.1.12", "macd.5.60", "macd.10.120",
                       "obv",
                       "wpr.10", "wpr.60", "wpr.120",
                       "cci.10", "cci.60", "cci.120",
                       "cmf.10", "cmf.60", "cmf.120",
                       "sar",
                       "garch")

  indicators <- matrix(NA, ncol=length(indicators.cols), nrow=nrow(prices))
  colnames(indicators)<- indicators.cols
  indicators <- as.xts(indicators, order.by=time(prices))
  d <- days[1]
  if (loading)
    pb.ind <- tkProgressBar(title = "Computing Indicators", min = 0,
                            max = length(days), width = 300)
  for (d in days){
    day.num <- which(days==d) # only for pb
    if (loading)
      setTkProgressBar(pb.ind, day.num, label=paste0(day.num, "/", length(days), " days computed"))
    else{
      cat(day.num/length(days)*100, "% \n")
    }
    prices.day <- prices[all.days==d,]
    #RSI
    indicators[all.days==d,]$rsi.10 <- as.numeric(RSI(prices.day$open, 10))
    indicators[all.days==d,]$rsi.60  <- as.numeric(RSI(prices.day$open, 60))
    indicators[all.days==d,]$rsi.120 <- as.numeric(RSI(prices.day$open, 120))

    # MACD
    indicators[all.days==d,]$macd.1.12 <-  as.numeric(MACD(prices.day$open, nfast=1, nSlow=10, nSig=1)[,1])/prices.day$open
    indicators[all.days==d,]$macd.5.60 <-  (as.numeric(MACD(prices.day$open, nfast=5, nSlow=60, nSig=1)[,1])/prices.day$open)
    indicators[all.days==d,]$macd.10.120 <-  (as.numeric(MACD(prices.day$open, nfast=10, nSlow=120, nSig=1)[,1])/prices.day$open)

    # WPR (needs to be lagged because it's computed on close price)
    wpr.10 <- as.numeric(WPR(prices.day[, c("high", "low", "close")], n=10))
    wpr.60 <- as.numeric(WPR(prices.day[, c("high", "low", "close")], n=60))
    wpr.120 <- as.numeric(WPR(prices.day[, c("high", "low", "close")], n=120))

    indicators[all.days==d,]$wpr.10 <- c(NA, wpr.10[-length(wpr.10)])
    indicators[all.days==d,]$wpr.60 <- c(NA, wpr.60[-length(wpr.60)])
    indicators[all.days==d,]$wpr.120 <- c(NA, wpr.120[-length(wpr.120)])

    #CCI (needs to be lagged because it's computed on close price)
    cci.10 <- as.numeric(CCI(prices.day[, c("high", "low", "close")], n=10, maType="EMA", c=0.015))
    cci.60 <- as.numeric(CCI(prices.day[, c("high", "low", "close")], n=60, maType="EMA", c=0.015))
    cci.120 <- as.numeric(CCI(prices.day[, c("high", "low", "close")], n=120, maType="EMA", c=0.015))

    indicators[all.days==d,]$cci.10 <- c(NA, cci.10[-length(cci.10)])
    indicators[all.days==d,]$cci.60 <- c(NA, cci.60[-length(cci.60)])
    indicators[all.days==d,]$cci.120 <- c(NA, cci.120[-length(cci.120)])

    # OBV
    indicators[all.days==d,]$obv <- as.numeric(OBV(prices.day$open, prices.day$volume))

    # returns
    indicators[all.days==d,]$return.1 <- ROC(as.numeric(prices.day$open), n=1, type="discrete")
    indicators[all.days==d,]$return.10 <- ROC(as.numeric(prices.day$open), n=10, type="discrete")
    indicators[all.days==d,]$return.30 <- ROC(as.numeric(prices.day$open), n=30, type="discrete")
    indicators[all.days==d,]$return.60 <- ROC(as.numeric(prices.day$open), n=60, type="discrete")
    indicators[all.days==d,]$return.120 <- ROC(as.numeric(prices.day$open), n=120, type="discrete")

    #CMF
    cmf.10 <- as.numeric(CMF(prices.day[,c("high", "low", "close")], as.numeric(prices.day$volume), n=10))
    cmf.60 <- as.numeric(CMF(prices.day[,c("high", "low", "close")], as.numeric(prices.day$volume), n=60))
    cmf.120 <- as.numeric(CMF(prices.day[,c("high", "low", "close")], as.numeric(prices.day$volume), n=120))

    indicators[all.days==d,]$cmf.10 <- c(NA, cmf.10[-length(cmf.10)])
    indicators[all.days==d,]$cmf.60  <- c(NA, cmf.60[-length(cmf.60)])
    indicators[all.days==d,]$cmf.120 <- c(NA, cmf.120[-length(cmf.120)])

    # Parabolic SAR
    indicators[all.days==d,]$sar <- as.numeric(SAR(prices.day[,c("high", "low")], accel=c(0.02, 0.2)))

    # GARCH
    indicators[all.days==d,]$garch <- garchFit(~garch(1,1), data=prices.day$open, trace=F)@sigma.t

    stopifnot(!any(colSums(!is.na(indicators[all.days==d,]))==0)) # check that all the inidcators could be computed for >0 obs
  }

  indicators$cmf.10[is.infinite(indicators$cmf.10)] <- mean(indicators$cmf.10[is.finite(indicators$cmf.10)])
  indicators$cmf.60[is.infinite(indicators$cmf.60)] <- mean(indicators$cmf.60[is.finite(indicators$cmf.60)])
  indicators$cmf.120[is.infinite(indicators$cmf.120)] <- mean(indicators$cmf.120[is.finite(indicators$cmf.120)])

  head(indicators)
  head(prices.day)

  if (loading)
    close(pb.ind)
  return(indicators)
}
