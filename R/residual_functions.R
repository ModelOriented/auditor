getResidualFunction <- function(residual.function){
  if (!is.null(residual.function)) {
    res <- function(model, y=NULL, data=NULL, predict.function=NULL){residual.function(model, data, y)}
  } else {
    res <- getResiduals
  }
  return(res)
}

getResiduals <- function(model, data=NULL, y = NULL, predict.function=NULL){
  possible_model_type <- try(model$type, silent = TRUE)
  if (class(possible_model_type) != "try-error" && !is.null(possible_model_type) && possible_model_type == "classification") {
    res <- 1 - predict.function(model, data)[cbind(1:nrow(data),y)]
  } else {
    if(is.factor(y)) y <- as.numeric(y) - 1
    res <-  y - predict.function(model, data)
  }

  return(res)
}
