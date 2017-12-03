
getResiduals <- function(model, y){
  y - predict(model)
}

getStdResiduals <- function(model, y){
  residuals <- y - predict(model)
  return(residuals / sd(residuals))
}
