getResiduals <- function(model, y, data, predict.function){

  if (!is.null(model$type) && model$type == "classification") {
    res <- 1 - predict.function(model, data)[cbind(1:nrow(data),y)]
  } else {
    if(is.factor(y)) y <- as.numeric(y) - 1
    res <-  y - predict(model, type = "response", newdata = data)
  }

  return(res)
}

#' @importFrom stats sd
getStdResiduals <- function(model, y, data, predict.function){
  res <- getResiduals(model, y, data, predict.function)
  return(res / sd(res))
}
