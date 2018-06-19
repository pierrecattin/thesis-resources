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
#'
#' @return returns nothing but prints the plot
#' @export
#'
freq.vs.accuracy.multiplot <- function (y.train, train.probs,
                                        y.dev, dev.probs,
                                        y.test, test.probs,
                                        conf, stock, horizon){

  plots <- list(freq.vs.accuracy.plot(y.train, train.probs, conf, "Training", legend=F),
     dev.plot <- freq.vs.accuracy.plot(y.dev, dev.probs, conf, "Dev", ytitle=F, legend=F),
     test.plot <- freq.vs.accuracy.plot(y.test, test.probs, conf, "Testing", ytitle=F))

  multiplot(plotlist = plots, cols=3,
   title=paste0("Frequency-Accuracy Tradeoff - ", stock, ", ",horizon, "min"))

}
