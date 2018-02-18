#' @title Score based on Cooks distance
#'
#' @description Cooks Distance TODO: defifnition here
#'
#' @param object An object of class ModelAudit
#'
#' @importFrom stats cooks.distance update
#'
#'
#' @export
#'

scoreCook <- function(object){
  if(object$model.class=="lm" || object$model.class == "glm"){
    return(  cooks.distance(object$model) )
  } else {
    return( computeScoreCook(object$model, object$data))
  }
}

computeScoreCook <- function(model, modelData){
  originalModel <- model
  n <- nrow(modelData)
  D <- numeric(n)
  y1 <- predict(originalModel)
  mse <- mean( (modelData[,1] - y1)^2 )
  p <- ncol(modelData)
  pmse <- p*mse
  for(i in 1:n){
    newModel <- update(originalModel, data = modelData[-i,])
    y2 <- predict(newModel, newdata = modelData)
    D[i] <- sum( (y1 - y2)^2 ) / (pmse)
    cat(i, "out of", n, "\r")
    utils::flush.console()
  }
  return(D)
}
