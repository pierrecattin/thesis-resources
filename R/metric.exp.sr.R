#' Metric expected SR
#'
#' @param y_true: tensor of shape (n obs, 1) containing true labels (1 or 0)
#' @param y_pred: tensor of shape (n obs, 1) containing predicted prob that obs is 1
#'
#' @return mean expected SR across min prob
#' @export
#'
metric.exp.sr <- function(y_true, y_pred){
  m <- tf$constant(60*5.5, dtype="float64")
  n <- tf$constant(8, dtype="float64")

  min.prob.seq <- tf$constant(seq(0.5, 1, 0.01), dtype="float32")

  preds <- tf$cast(y_pred>=min.prob.seq, "int32") -
    tf$cast(y_pred<(1-min.prob.seq), "int32") # matrix where each line is an observation and each column is one min prob level

  buy.logi <- preds== 1L # n.obs x n.min.prob
  sell.logi <- preds== -1L

  wins <- (buy.logi & (y_true==1L)) | (sell.logi & (y_true==0L)) # n.obs x n.min.prob
  n.wins <- tf$reduce_sum(tf$cast(wins, "int32"), axis=0L, keepdims=T) # colsums: number of wins for each min.prob
  n.trades <- tf$reduce_sum(tf$cast(buy.logi, "int32")+tf$cast(sell.logi, "int32"), axis=0L, keepdims=T) # 1 x n.min.prob
  p <- n.wins/n.trades # accuracy by min.votes

  n.pred <- tf$shape(y_pred)[1] # scalar
  q <- n.trades/n.pred # frequency by min.votes
  one <- tf$constant(1, dtype="float64");   two <-  tf$constant(2, dtype="float64"); four <-  tf$constant(4, dtype="float64")
  exp.sr <-  sqrt(q*n*m)*(two*p-one) / sqrt(-four*p^2*q+four*p*q-q+one) # n.obs x n.min.prob

  exp.sr.zero <- tf$where(tf$is_nan(exp.sr), tf$zeros_like(exp.sr), exp.sr) # replace NaN by 0
  n.sr <- tf$shape(exp.sr, out_type="float64")[2] - tf$reduce_sum(tf$cast(tf$is_nan(exp.sr), "float64")) # number of non-na elements in exp.sr
  exp.sr.mean <- tf$reduce_sum(exp.sr.zero, axis=1L)/n.sr
  return(exp.sr.mean)
}
