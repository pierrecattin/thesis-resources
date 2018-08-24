#' Compute Price Direction
#' Computes a variable describing the direction of future price changes
#'
#' @param prices xts where one numeric column has to be called "open" and one "close"
#' @param daily.start DEPRECATED (which observation number should be the first one for which the variable is computed each day; numeric)
#' @param horizon prediction horizon; numeric
#' @param min.return  minimum absolute return in horizon to be classified as -1 or +1; default=0; numeric
#' @param loading logical telling if loading bar should be displayed
#'
#' @return xts with one numeric (values -1, 0, +1, NA) column and same indices as prices. last 'horizon' values of each day are NA
#' @export
#'
compute.price.direction <- function (prices, daily.start=1, horizon, min.return=0, loading=T){
  all.days <- as.Date(time(prices))
  days <- unique(all.days)

  # initiate y variable as NA with same time indexes as prices
  y <- prices$close
  colnames(y)<-"movement"
  y$movement <- NA

  n.obs.daily <- sum(all.days==all.days[1])
  d <- days[1]
  if (loading)
    pb.mov <- tkProgressBar(title = "Computing y-Variable", min = 0,
                          max = length(days), width = 300)
  for (d in days){
    if (loading) {
      day.num <- which(days==d) # only for pb
      setTkProgressBar(pb.mov, day.num, label=paste0(day.num, "/", length(days), " days computed"))
    }
    closes <- prices[all.days==d,]$close
    opens <- prices[all.days==d,]$open
    if (length(closes) != n.obs.daily){
      warning(paste0("The number of observations on ", d," is different from the other days. This day will be ignored"))
    } else{
      future.closes <- c(as.numeric(closes[horizon:nrow(closes),]),
                         rep(NA, horizon-1)) # closes horizon-1 minutes ahead
      up <- 1*(future.closes >= opens*(1+min.return))
      down <- 1*(future.closes < opens*(1-min.return))
      y$movement[all.days==d,] <- up - down
    }
  }
  if (loading)
    close(pb.mov)
  return(y)
}
