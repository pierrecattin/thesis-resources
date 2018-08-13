#' Frequency vs accuracy of prediction for all 3 sets and all horizons
#'
#' @param freq.acc data frame with columns min.prob, Accuracy, Frequency, Set, Horizon, lower, upper
#' @param conf numeric indicating if confidence intervall to be plotted (only for legend). !!! It must be the same as the one used to compute freq.acc, else there's a missmatch between plot and legend
#' @param stock stock name for title
#' @param model model name for title
#' @param support.color logical indicating if lines should be colored based on support level
#'
#' @return 4 by 3 facets ggplot
#' @export
#'
freqacc.byset.byhoriz.plot <- function (freq.acc, conf, stock, model, support.color=F){

  # sort Set and Horizon to get the correct arrangement of the facets
  freq.acc$Set <- factor(freq.acc$Set, levels=c("Training Set", "Validation Set", "Testing Set"))

  horizon.levels <- unique(freq.acc$Horizon)
  horizon.levels <- horizon.levels[order(horizon.levels)] # making sure that the levels are correctly sorted
  horizon.levels <- paste(horizon.levels, "min")
  freq.acc$Horizon <- factor(paste(freq.acc$Horizon, "min"), levels=horizon.levels)

  # library(extrafont)
  # loadfonts(device = "win")
  my.plot <- ggplot(data=freq.acc, aes(x=Frequency, y=Accuracy)) +
    facet_grid(Horizon~Set) +
    theme_bw(base_size=13, base_family="serif")

  if(!missing(conf)){
    my.plot <- my.plot + geom_ribbon(data=freq.acc, aes(ymin=lower,ymax=upper, x=Frequency, fill = "red"), alpha=0.2) +
      scale_fill_manual( "", labels = paste0(conf*100,"% confidence interval"), values=c("red"="red"))
  }

  if(support.color){
    my.plot <- my.plot +
      geom_line(aes(color=round(min.prob, 1)),size=1)  +
      scale_colour_continuous(name="Minimum support", trans="reverse", low = "#000066", high = "#ccccff")# legend of min.prob
  } else{
    my.plot <- my.plot +
      geom_line()
  }
  my.plot <- my.plot+
    geom_hline(yintercept=0.5, color="darkblue") +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_y_continuous(labels = scales::percent)+
    theme(legend.position="bottom") +
    ggtitle(paste0("Frequency and Accuracy - ",model,", ", stock))
    #ggtitle("Frequency-Accuracy Tradeoff", subtitle=paste0(model,", ", stock))
    #ggtitle(paste0(model,", ", stock))

  return(my.plot)
}
