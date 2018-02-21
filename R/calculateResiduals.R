getResiduals <- function(model, y){
  y - predict(model, type = "response")
}

#' @importFrom stats sd
getStdResiduals <- function(model, y){
  residuals <- y - predict(model)
  return(residuals / sd(residuals))
}
