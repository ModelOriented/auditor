getResiduals <- function(model, y){
  if(!is.numeric(y)) y <- as.numeric(as.character(y))
  y - predict(model, type = "response")
}

#' @importFrom stats sd
getStdResiduals <- function(model, y){
  if(!is.numeric(y)) y <- as.numeric(as.character(y))
  residuals <- y - predict(model)
  return(residuals / sd(residuals))
}
