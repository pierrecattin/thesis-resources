#' Plot frequency vs accuracy of prediction
#'
#' @param y  : factor with levels -1, 1
#' @param probs : matrix with 2 columns: first is probability of up, second is down
#' @param conf: numeric indicating if confidence intervall to be plotted (if missing, no intervall is plotted)
#' @param stock: character (used for title)
#' @param horizon: character or numeric (used for title)
#'
#' @return Named numeric containing the results of the best value for min.prop: c(min.prob, dev.accuracy, dev.freq, dev.sr)
#' @export
#'
plot.freq.vs.accuracy <- function (y, probs, conf, stock="stock", horizon){
  stopifnot(ncol(probs)==2)
  stopifnot(length(y)==nrow(probs))
  stopifnot(all.equal(levels(y), c("-1","1")))

  min.prob.seq <- seq(0.5, 1, 0.001)
  data <- data.frame(min.prob=min.prob.seq, Accuracy=rep(NA, length(min.prob.seq)), Frequency=rep(NA, length(min.prob.seq)))
  for (min.prob in min.prob.seq){
    results <- evaluate.predictions(y, probs, min.prob)
    if(results[1]=="no predictions") { # if no predictions are made, break the loop
      break
    }
    data[which(min.prob.seq==min.prob), c("Accuracy", "Frequency")] <- results[c("accuracy", "freq")]
  }
  data <- data[!is.na(data$Accuracy), ]
  n.small <- nrow(probs) * data$Frequency * data$Accuracy < 5 | nrow(probs) * data$Frequency * (1-data$Accuracy) < 5
  data <- data[!n.small, ] # remove datapoints with little observations

  plot <- ggplot(data=data, aes(x=Frequency, y=Accuracy)) +
    geom_line(aes(color=min.prob))  +
    ggtitle(paste0("Accuracy vs frequency of preditions - ", stock,", ", horizon, "min horizon")) +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_y_continuous(labels = scales::percent)+
    scale_colour_continuous(name="Min prob.", trans="reverse") + # reverse legend of min.prob
    theme_bw()+
    theme(legend.position="bottom")

  if(!missing(conf)){
    bounds <- confidence.bound(p=data$Accuracy, n=nrow(probs)*data$Frequency)
    data <- cbind(data, bounds)
    plot <- plot + geom_ribbon(data=data, aes(ymin=lower,ymax=upper, x=Frequency, fill = "darkred"), alpha=0.1) +
      scale_fill_manual( "", labels = paste0(conf*100,"% confidence intervall"), values=c("darkred"="darkred"))
  }
  return(plot)
}

# y <- y.dev
# probs <- dev.probs
# min.prob <- 0.6
# library(ggplot2)
# stock <- "AAPL"
# set <- "dev"
