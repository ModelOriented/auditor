getResiduals <- function(model, y, data){
  if ("randomForest" %in% class(model)) {
    res <- 1 - predict(model, type = "prob", newdata = data)[cbind(1:nrow(data),y)]
  } else {
    res <-  y - predict(model, type = "response", newdata = data)
  }
  return(res)
}

#' @importFrom stats sd
getStdResiduals <- function(model, y, data){
  if("randomForest" %in% class(model)) {
    res <- 1 - predict(model, type = "prob", newdata = data)[cbind(1:nrow(data),y)]
  } else {
    res <- y - getResiduals(model, y, data)
  }
  return(res / sd(res))
}
