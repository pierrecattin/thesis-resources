#' Frequency vs accuracy of prediction for all 3 sets and all horizons
#'
#' @param spec.sens data frame with columns Specificity, Sensitivity, Set, Horizon
#' @param stock stock name for title
#' @param model model name for title
#'
#' @return 4 by 3 facets ggplot
#' @export
#'
roc.byset.byhoriz.plot <- function (spec.sens, stock, model){

  #spec.sens.sub <- spec.sens[spec.sens$Set=="Training Set" & spec.sens$Horizon=="1 min",]

  # sort Set and Horizon to get the correct arrangement of the facets
  spec.sens$Set <- factor(spec.sens$Set, levels=c("Training Set", "Validation Set", "Testing Set"))

  horizon.levels <- unique(spec.sens$Horizon)
  horizon.levels <- horizon.levels[order(horizon.levels)] # making sure that the levels are correctly sorted
  horizon.levels <- paste(horizon.levels, "min")
  spec.sens$Horizon <- factor(paste(spec.sens$Horizon, "min"), levels=horizon.levels)

  spec.sens$FPR <- 1-spec.sens$Specificity

  my.plot <- ggplot(data=spec.sens, aes(x=FPR, y=Sensitivity)) +
    facet_grid(Horizon~Set) +
    theme_bw(base_size=13, base_family="serif")+
    geom_line()

  my.plot <- my.plot+
    geom_abline(intercept=0, slope=1, color="blue", linetype="dotted") +
    scale_x_continuous(name="False Positive Rate", limits = c(0, 1), labels = scales::percent) +
    scale_y_continuous(name="True Positive Rate", labels = scales::percent)+
    ggtitle(paste0("ROC Curves - ",model,", ", stock))

  return(my.plot)
}
