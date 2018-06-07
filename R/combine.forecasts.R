#' Combine FOrecasts (WIP)
#' Combine forecasts of various classifiers into one single forecast
#'
#' @param x: matrix where each line is an observation, and each colum contains the probability that the class is "positive "up" estimated by one model. Colnames are the name of the models.
#' @param y: vector of true classes
#'
#' @return
#' @export list containing vector of combined probs and named numerics of coefficients
#'
#'
combine.forecasts <- function(x, y){
  stopifnot(nrow(x) == length(y))
  data <- cbind(y, x)
  data <- data.frame(data)
  colnames(data)<-c("y", colnames(x))
  # prior <- rep(0.5,2)
  # lda.fit <- lda(y~., data=data, prior=prior)
  # probs <- predict(lda.fit, newdata=data.frame(x))$posterior[,2:1]
  # # yhat <- (probs[,1]>=0.5)*2-1; sum(yhat!=y)/length(y)
  # coeffs <- lda.fit$scaling
  # scores <- x %*% matrix(coeffs, ncol=1)
  # plot(scores, probs[,1])
}


# x<- matrix(rnorm(10000*4), ncol=4)
# y <- ((x[,1]+2*x[,2]-x[,3]-4*x[,4])>0)*2-1
# #y <- ((x[,1])>0)*2-1
# colnames(x)<-paste0("model_",1:ncol(x))
# plot(y, x[,1])
#
# head(log(1.657558*x))
# head(probs)
