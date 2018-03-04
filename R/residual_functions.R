getResidualFunction <- function(residual.function){
  if (!is.null(residual.function)) {
    res <- function(model, y=NULL, data=NULL, predict.function=NULL){residual.function(model, y)}
  } else {
    res <- getResiduals
  }
  return(res)
}

getResiduals <- function(model, y=NULL, data=NULL, predict.function=NULL){

  if (!is.null(model$type) && model$type == "classification") {
    res <- 1 - predict.function(model, data)[cbind(1:nrow(data),y)]
  } else {
    if(is.factor(y)) y <- as.numeric(y) - 1
    res <-  y - predict.function(model, data)
  }

  return(res)
}
