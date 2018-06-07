#' Compute Indicators
#' Computes technical indicators based on OLHCV prices series
#'
#' @param prices OLHCV prices series; xts
#'
#' @return xts with one column per technical indicator. Index is same as index of prices.
#' @export
#'
compute.indicators <- function(prices){

  all.days <- as.Date(time(prices))
  days <- unique(all.days)

  indicators.cols <- c("rsi.10", "rsi.60", "rsi.120",
                       "macd.1.12", "macd.5.60", "macd.10.120",
                       "obv",
                       "wpr.10", "wpr.60", "wpr.120",
                       "cci.10", "cci.60", "cci.120",
                       "return.1", "return.5", "return.15", "return.60", "return.120")
  indicators <- matrix(NA, ncol=length(indicators.cols), nrow=nrow(prices))
  colnames(indicators)<- indicators.cols
  indicators <- as.xts(indicators, order.by=time(prices))
  # d <- days[1]
  pb.ind <- tkProgressBar(title = "Computing Indicators", min = 0,
                          max = length(days), width = 300)
  for (d in days){
    day.num <- which(days==d) # only for pb
    setTkProgressBar(pb.ind, day.num, label=paste0(day.num, "/", length(days), " days computed"))
    prices.day <- prices[all.days==d,]
    #RSI
    indicators[all.days==d,]$rsi.10 <- as.numeric(RSI(prices.day$Close, 10))
    indicators[all.days==d,]$rsi.60  <- as.numeric(RSI(prices.day$Close, 60))
    indicators[all.days==d,]$rsi.120 <- as.numeric(RSI(prices.day$Close, 120))

    # MACD
    indicators[all.days==d,]$macd.1.12 <-  as.numeric(MACD(prices.day$Close, nfast=1, nSlow=10, nSig=1)[,1])/prices.day$Close
    indicators[all.days==d,]$macd.5.60 <-  (as.numeric(MACD(prices.day$Close, nfast=5, nSlow=60, nSig=1)[,1])/prices.day$Close)
    indicators[all.days==d,]$macd.10.120 <-  (as.numeric(MACD(prices.day$Close, nfast=10, nSlow=120, nSig=1)[,1])/prices.day$Close)

    # WPR
    indicators[all.days==d,]$wpr.10 <- as.numeric(WPR(prices.day[, c("High", "Low", "Close")], n=10))
    indicators[all.days==d,]$wpr.60 <- as.numeric(WPR(prices.day[, c("High", "Low", "Close")], n=60))
    indicators[all.days==d,]$wpr.120 <- as.numeric(WPR(prices.day[, c("High", "Low", "Close")], n=120))

    #CCI
    indicators[all.days==d,]$cci.10 <- as.numeric(CCI(prices.day[, c("High", "Low", "Close")], n=10, maType="EMA", c=0.015))
    indicators[all.days==d,]$cci.60 <- as.numeric(CCI(prices.day[, c("High", "Low", "Close")], n=60, maType="EMA", c=0.015))
    indicators[all.days==d,]$cci.120 <- as.numeric(CCI(prices.day[, c("High", "Low", "Close")], n=120, maType="EMA", c=0.015))


    # OBV
    indicators[all.days==d,]$obv <- as.numeric(OBV(prices.day$Close, prices.day$Volume))


    # returns
    indicators[all.days==d,]$return.1 <- ROC(as.numeric(prices.day$Close), n=1, type="discrete")
    indicators[all.days==d,]$return.5 <- ROC(as.numeric(prices.day$Close), n=5, type="discrete")
    indicators[all.days==d,]$return.15 <- ROC(as.numeric(prices.day$Close), n=15, type="discrete")
    indicators[all.days==d,]$return.60 <- ROC(as.numeric(prices.day$Close), n=60, type="discrete")
    indicators[all.days==d,]$return.120 <- ROC(as.numeric(prices.day$Close), n=120, type="discrete")
  }
  close(pb.ind)
  return(indicators)
}
