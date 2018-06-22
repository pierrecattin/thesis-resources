#' Frequency vs accuracy of prediction for all 3 samples
#'
#' @param y.train numeric -1, +1
#' @param train.probs numeric indicating probability of up
#' @param y.dev numeric -1, +1
#' @param dev.probs numeric indicating probability of up
#' @param y.test numeric -1, +1
#' @param test.probs numeric indicating probability of up
#' @param conf numeric indicating if confidence intervall to be plotted
#' @param stock stock name for title
#' @param horizon horizon in minutes for title
#' @param model model name for title
#' @param granularity numeric indicating granularity for the sequence of min supports
#'
#' @return ggplot
#' @export
#'
freq.vs.accuracy.multiplot <- function (y.train, train.probs,
                                        y.dev, dev.probs,
                                        y.test, test.probs,
                                        conf, stock, model, horizon, granularity=0.01){

  data <- rbind(get.acc.freq(y.train, train.probs, granularity, "Training Set", horizon, conf),
                get.acc.freq(y.dev, dev.probs, granularity, "Validation Set", horizon, conf),
                get.acc.freq(y.test, test.probs, granularity, "Testing Set", horizon, conf))

  data$Set <- factor(data$Set, levels=c("Training Set", "Validation Set", "Testing Set"))
  my.plot <- ggplot(data=data, aes(x=Frequency, y=Accuracy)) +
    geom_line(aes(color=round(min.prob, 1)),size=1.2)  +
    facet_wrap(~Set) +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_y_continuous(labels = scales::percent)+
    scale_colour_continuous(name="Minimum support", trans="reverse") + # reverse legend of min.prob
    theme_bw(base_size=10) +
    #  theme_bw(base_size=12,base_family="Times New Roman") +
    theme(legend.position="bottom") +
    ggtitle(paste0("Frequency-Accuracy Tradeoff : ",model,", ", stock, ", ",horizon, "min"))

  if(!missing(conf)){
    my.plot <- my.plot + geom_ribbon(data=data, aes(ymin=lower,ymax=upper, x=Frequency, fill = "darkred"), alpha=0.2) +
      scale_fill_manual( "", labels = paste0(conf*100,"% confidence interval"), values=c("darkred"="darkred"))
  }
  return(my.plot)
}
