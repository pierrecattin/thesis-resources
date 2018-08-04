#' Train feed-forward ANN
#'
#' @param x.train matrix/xts n.obs*n.features
#' @param x.dev matrix/xts n.obs*n.features
#' @param y.train factor with levels c(-1, 1)
#' @param y.dev factor with levels c(-1, 1)
#' @param epochs number of eposch
#' @param batch.size min batch size
#' @param activations activation functions used in hidden layer: c("relu", "tanh", ...). For the output layer, softmax is used
#' @param structure numeric vector indicating number of nodes in each hidden layer
#' @param l2 l2 regularisation value
#' @param learning.rate learning rate
#' @param tb.log character inidicating run name for tensorflow. if missing, tensorboard is not ran
#' @param perf.metric c("mean.sr", "mean.accuracy")
#'
#' @return fit
#' @export
#'
train.nn <- function(x.train, x.dev, y.train, y.dev,
                     epochs, batch.size, activations, structure,
                     l2, learning.rate, tb.log,
                     perf.metric){
  y.train <- matrix(as.numeric(y.train), ncol=1)-1 # 0 if down, 1 if up
  y.dev <- matrix(as.numeric(y.dev, ncol=1))-1

  model <- keras_model_sequential()
  for(i in 1:length(structure)){ # add hidden layers
    if (i == 1){
      input.shape <- ncol(x.train)
    } else {
      input.shape <- structure[i-1]
    }
    layer_dense(model, units = structure[i], activation = activations, input_shape = input.shape,
                kernel_regularizer=regularizer_l2(l2))
  }
  layer_dense(model, units = 1, activation = "sigmoid") # output layer

  if(perf.metric=="mean.sr"){
    compile(model,
            loss = "binary_crossentropy",
            optimizer = optimizer_adam(lr=learning.rate),
            metrics = c("accuracy", metric.exp.sr))
  } else if (perf.metric=="mean.accuracy"){
    compile(model,
            loss = "binary_crossentropy",
            optimizer = optimizer_adam(lr=learning.rate),
            metrics = c("accuracy", metric.mean.accuracy))
  } else {
    stop("unknown perf.metric")
  }

  if(missing(tb.log)) {
    fit(model,
        x = x.train,
        y = y.train,
        batch_size = batch.size,
        epochs = epochs,
        verbose = 1,
        validation_data =  list(x.dev, y.dev))
  } else {
    tensorboard(paste0("tb_logs/",tb.log))
    fit(model,
        x = x.train,
        y = y.train,
        batch_size = batch.size,
        epochs = epochs,
        verbose = 1,
        validation_data =  list(x.dev, y.dev),
        callbacks = callback_tensorboard(paste0("tb_logs/",tb.log)))
  }
  return(model)
}
