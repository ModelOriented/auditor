#' @title Score based on Cooks distance
#'
#' @description Cook’s distance are used for estimate of the influence of an single observation.
#'
#' @param object An object of class ModelAudit
#' @param print If TRUE progress is printed
#'
#' @details Cook’s distance is a tool for identifying observations that may negatively affect the model.
#' They may be also used for indicating regions of the design space where it would be good to obtain more observations.
#' Data points indicated by Cook’s distances are worth checking for validity.
#'
#' Cook’s Distances are calculated by removing the i-th observation from the data and recalculating the model.
#' It shows how much all the values in the model change when the i-th observation is removed.
#'
#' Models of classes other than lm and glm the distances are computed directly from the definition,
#' so this may take a while. In this example we will compute them for a linear model.
#'
#' @importFrom stats cooks.distance update
#'
#' @return numeric vector
#'
#' @export
#'

scoreCook <- function(object, print=TRUE){
  if(object$model.class=="lm" || object$model.class == "glm"){
    return(  cooks.distance(object$model) )
  } else {
    return( computeScoreCook(object$model, object$data, print))
  }
}

computeScoreCook <- function(model, modelData, print){
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
    if(print==TRUE) cat(i, "out of", n, "\r")
    utils::flush.console()
  }


  return(D)
}
