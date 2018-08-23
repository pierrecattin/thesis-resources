#' DEPRECATED: Compute Price Direction from close to close
#' Computes a variable describing the direction of future price changes
#'
#' @param prices xts where one numeric column has to be called "close"
#' @param daily.start which observation number should be the first one for which the variable is computed each day; numeric
#' @param horizon prediction horizon; numeric
#' @param min.return  minimum absolute return in horizon to be classified as -1 or +1; default=0; numeric
#' @param loading logical telling if loading bar should be displayed
#'
#' @return xts with one numeric (values -1, 0, +1, NA) column and same indices as prices. last 'horizon' values of each day are NA
#' @export
#'
compute.prices.direction.close.to.close <- function (prices, daily.start, horizon, min.return=0, loading=T){
  all.days <- as.Date(time(prices))
  days <- unique(all.days)

  y <- prices$close
  colnames(y)<-"movement"
  y$movement <- NA

  n.obs.daily <- sum(all.days==all.days[1])
  # d <- days[1]
  if (loading)
    pb.mov <- tkProgressBar(title = "Computing y-Variable", min = 0,
                          max = length(days), width = 300)
  for (d in days){
    if (loading) {
      day.num <- which(days==d) # only for pb
      setTkProgressBar(pb.mov, day.num, label=paste0(day.num, "/", length(days), " days computed"))
    }
    closes <- prices[all.days==d,]$close
    if (nrow(closes) != n.obs.daily){
      warning(paste0("The number of observations on the ", d," is different from the other days. This day will be ignored"))
    } else{
      future.closes <- c(rep(NA, daily.start-1),
                         as.numeric(closes[(daily.start+horizon):nrow(closes),]),
                         rep(NA, horizon))
      up <- 1*(future.closes>=closes*(1+min.return))
      down <- 1*(future.closes<closes*(1-min.return))
      y$movement[all.days==d,] <- up - down
    }
  }
  if (loading)
    close(pb.mov)
  return(y)
}
