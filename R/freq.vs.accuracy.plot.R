#' Plot frequency vs accuracy of prediction
#'
#' @param y  numeric -1, +1
#' @param probs numeric indicating probability of up
#' @param conf numeric indicating if confidence intervall to be plotted (if missing, no intervall is plotted)
#' @param set character (used for title)
#' @param ytitle logical indicating if y title should be diplayed
#' @param legend logical indicating if y legend should be displayed
#'
#' @return ggplot
#' @export
#'
freq.vs.accuracy.plot <- function (y, probs, conf, set, ytitle=T, legend=T){
  stopifnot(length(y)==length(probs))

  min.prob.seq <- seq(0.5, 1, 0.025)
  data <- data.frame(min.prob=min.prob.seq, Accuracy=rep(NA, length(min.prob.seq)), Frequency=rep(NA, length(min.prob.seq)))
  #min.prob <- .6
  for (min.prob in min.prob.seq){
    results <- evaluate.predictions(y, probs, min.prob, n=8)
    if(results[1]=="no predictions") { # if no predictions are made, break the loop
      break
    }
    data[which(min.prob.seq==min.prob), c("Accuracy", "Frequency")] <- results[c("accuracy", "freq")]
  }
  data <- data[!is.na(data$Accuracy), ]
  n.small <- length(probs) * data$Frequency  < 100
  data <- data[!n.small, ] # remove datapoints with little observations


  my.plot <- ggplot(data=data, aes(x=Frequency, y=Accuracy)) +
    geom_line(aes(color=round(min.prob, 1)),size=1.2)  +
    ggtitle(paste(set,"Set")) +
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
    my.plot <- my.plot + geom_ribbon(data=data, aes(ymin=lower,ymax=upper, x=Frequency, fill = "darkred"), alpha=0.2) +
      scale_fill_manual( "", labels = paste0(conf*100,"% confidence intervall"), values=c("darkred"="darkred"))
  }
  if(!ytitle)
    my.plot <-  my.plot + ylab(NULL)

  if(!legend) {
    my.plot <-  my.plot + guides(fill=FALSE) # confidence legend
    #my.plot <-  my.plot + guides(color=FALSE) # min support legend
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
