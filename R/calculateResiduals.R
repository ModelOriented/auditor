getResiduals <- function(model, y, data){
  if (is.numeric(y)) {
    res <-  y - predict(model, type = "response", newdata = data)
  } else {
    res <- 1 - predict(model, type = "prob", newdata = data)[cbind(1:nrow(data),y)]
  }
  return(res)
}

#' @importFrom stats sd
getStdResiduals <- function(model, y, data){
  if(is.numeric(y)) {
    res <- y - getResiduals(model, y, data)
  } else {
    res <- 1 - predict(model, type = "prob", newdata = data)[cbind(1:nrow(data),y)]
  }
  return(res / sd(res))
}
