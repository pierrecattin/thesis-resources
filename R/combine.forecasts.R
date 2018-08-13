#' Combine Forecasts
#' Combine forecasts of various classifiers into one single forecast
#'
#' @param predictions: matrix where each column contain predictions (up probability) for a model.
#' @param y: vector of true classes (+1 / -1)
#'
#' @return list of 2 containing vector of combined probs and fit
#' @export
#'
combine.forecasts <- function(predictions.dev, predictions.test, y.dev){
  stopifnot(nrow(predictions.dev) == length(y.dev))

  fit <- glm(y.dev~.,data.frame(predictions.dev), family=binomial)

  probs.dev <-  as.numeric(predict(object=fit, newdata=data.frame(predictions.dev), type="response"))
  probs.test <-  as.numeric(predict(object=fit, newdata=data.frame(predictions.test), type="response"))

  return(list(probs.dev=probs.dev, probs.test=probs.test, fit=fit))
}
