#' Plot frequency vs accuracy of prediction
#'
#' @param y  factor with levels -1, 1
#' @param probs numeric indicating probability of up
#' @param conf numeric indicating if confidence intervall to be plotted (if missing, no intervall is plotted)
#' @param stock character (used for title)
#' @param horizon character or numeric (used for title)
#' @param set character (used for title)
#' @param n number of stocks
#'
#' @return ggplot
#' @export plot.freq.vs.accuracy
#'
plot.freq.vs.accuracy <- function (y, probs, conf, stock="stock", horizon, set, n){
  stopifnot(length(y)==length(probs))
  stopifnot(all.equal(levels(y), c("-1","1")))

  min.prob.seq <- seq(0.5, 1, 0.001)
  data <- data.frame(min.prob=min.prob.seq, Accuracy=rep(NA, length(min.prob.seq)), Frequency=rep(NA, length(min.prob.seq)))
  for (min.prob in min.prob.seq){
    results <- evaluate.predictions.num(y, probs, min.prob, n)
    if(results[1]=="no predictions") { # if no predictions are made, break the loop
      break
    }
    data[which(min.prob.seq==min.prob), c("Accuracy", "Frequency")] <- results[c("accuracy", "freq")]
  }
  data <- data[!is.na(data$Accuracy), ]
  n.small <- length(probs) * data$Frequency  < 20
  data <- data[!n.small, ] # remove datapoints with little observations


  my.plot <- ggplot(data=data, aes(x=Frequency, y=Accuracy)) +
    geom_line(aes(color=round(min.prob, 1)))  +
    ggtitle(paste0("accuracy vs frequency of predictions (", stock,", ", set, ", ", horizon, "min,)")) +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_y_continuous(labels = scales::percent)+
    scale_colour_continuous(name="Min support", trans="reverse") + # reverse legend of min.prob
    theme_bw(base_size=10) +
    #  theme_bw(base_size=12,base_family="Times New Roman") +
    theme(legend.position="bottom")

  if(!missing(conf)){
    bounds <- confidence.bound(p=data$Accuracy, n=length(probs)*data$Frequency)
    bounds[bounds[,1]<0,1] <- 0
    bounds[bounds[,1]>1,2] <- 1
    data <- cbind(data, bounds)
    my.plot <- my.plot + geom_ribbon(data=data, aes(ymin=lower,ymax=upper, x=Frequency, fill = "darkred"), alpha=0.1) +
      scale_fill_manual( "", labels = paste0(conf*100,"% confidence intervall"), values=c("darkred"="darkred"))
  }
  return(my.plot)
}

# y <- y.dev
# probs <- dev.probs
# min.prob <- 0.6
# library(ggplot2)
# stock <- "AAPL"
# set <- "dev"
# conf <- 0.95
