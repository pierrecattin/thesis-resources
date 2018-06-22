#' Plot frequency vs accuracy of prediction. Single plot
#'
#' @param y  numeric -1, +1
#' @param probs numeric indicating probability of up
#' @param conf numeric indicating if confidence intervall to be plotted (if missing, no intervall is plotted)
#' @param set character (used for title)
#' @param ytitle logical indicating if y title should be diplayed
#' @param legend logical indicating if y legend should be displayed
#' @param granularity numeric indicating granularity for the sequence of min support
#'
#' @return ggplot
#' @export
#'
freq.vs.accuracy.plot <- function (y, probs, conf, set, ytitle=T, legend=T, granularity){
  stopifnot(length(y)==length(probs))

  data <- get.acc.freq(y, probs, granularity)


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
