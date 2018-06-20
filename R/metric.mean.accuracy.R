#' Metric mean accuracy across min probs
#'
#' @param y_true: tensor of shape (n obs, 1) containing true labels (1 or 0)
#' @param y_pred: tensor of shape (n obs, 1) containing predicted prob that obs is 1
#'
#' @return mean accuracy across min prob
#' @export
#'
metric.mean.accuracy <- function(y_true, y_pred){
  min.prob.seq <- tf$constant(seq(0.5, 1, 0.01), dtype="float32")

  preds <- tf$cast(y_pred>=min.prob.seq, "int32") -
    tf$cast(y_pred<(1-min.prob.seq), "int32") # matrix where each line is an observation and each column is one min prob level

  buy.logi <- preds == 1L # n.obs x n.min.prob
  sell.logi <- preds == -1L

  wins <- (buy.logi & (y_true==1L)) | (sell.logi & (y_true==0L)) # n.obs x n.min.prob
  n.wins <- tf$reduce_sum(tf$cast(wins, "int32"), axis=0L, keepdims=T) # colsums: number of wins for each min.prob
  n.trades <- tf$reduce_sum(tf$cast(buy.logi, "int32")+tf$cast(sell.logi, "int32"), axis=0L, keepdims=T) # 1 x n.min.prob
  accuracy <- n.wins/n.trades # accuracy by min.votes

  accuracy.zeros <- tf$where(tf$is_nan(accuracy), tf$zeros_like(accuracy), accuracy) # replace NaN by 0
  n.accuracies <- tf$shape(accuracy, out_type="float64")[2] - tf$reduce_sum(tf$cast(tf$is_nan(accuracy), "float64")) # number of non-na elements in exp.sr
  mean.accuracy <- tf$reduce_sum(accuracy.zeros, axis=1L)/n.accuracies
  return(mean.accuracy)
}


# sess <- tf$Session()
# sess$run(accuracy.zeros)
# sess$close()
