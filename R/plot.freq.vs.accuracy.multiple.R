#' Plot frequency vs accuracy of prediction for all 3 samples
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
#'
#' @return ggplot
#' @export
#'
plot.freq.vs.accuracy.multiple <- function (y.train, train.probs,
                                   y.dev, dev.probs,
                                   y.test, test.probs,
                                   conf, stock, horizon){

  plots <- list(plot.freq.vs.accuracy(y.train, train.probs, conf, "Training", legend=F),
      dev.plot <- plot.freq.vs.accuracy(y.dev, dev.probs, conf, "Dev", ytitle=F, legend=F),
      test.plot <- plot.freq.vs.accuracy(y.test, test.probs, conf, "Testing", ytitle=F))

  my.plot <- multiplot(plotlist = plots, cols=3,
    title=paste0("Frequency-Accuracy Tradeoff - ", stock, ", ",horizon, "min"))
}
