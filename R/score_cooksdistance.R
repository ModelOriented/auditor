#' @title Score based on Cooks Distance
#'
#' @description Cook’s distance are used for estimate of the influence of an single observation.
#'
#' @param object An object of class 'explainer' created with function \code{\link[explain]{DALEX}} from the DALEX package.
#' @param print If TRUE progress is printed.
#'
#' @details Cook’s distance is a tool for identifying observations that may negatively affect the model.
#' They may be also used for indicating regions of the design space where it would be good to obtain more observations.
#' Data points indicated by Cook’s distances are worth checking for validity.
#'
#' Cook’s Distances are calculated by removing the i-th observation from the data and recalculating the model.
#' It shows how much all the values in the model change when the i-th observation is removed.
#'
#' Models of classes other than lm and glm the distances are computed directly from the definition,
#' so this may take a while.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' score_cooksdistance(lm_au)
#'
#'
#' @importFrom stats cooks.distance update
#'
#' @seealso \code{\link{score}}
#'
#' @return numeric vector
#'
#' @export
#'

score_cooksdistance <- function(object, print=TRUE){
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  if(any(object$class=="lm") || any(object$class == "glm")) {
    return(  cooks.distance(object$model) )
  } else {
    return( compute_cooksdistances(object, print))
  }
}

compute_cooksdistances <- function(object, print){
  original_model <- object$model
  model_data <- object$data
  predict_function <- object$predict_function
  n <- nrow(model_data)
  D <- numeric(n)
  y1 <- predict_function(original_model)
  mse <- mean( (as.numeric(model_data[,1]) - y1)^2 )
  p <- ncol(model_data)
  pmse <- p*mse
  for(i in 1:n){
    new_model <- update(original_model, data = model_data[-i,])
    y2 <- predict_function(new_model, newdata = model_data)
    D[i] <- sum( (y1 - y2)^2 ) / (pmse)
    if(print==TRUE) cat(i, "out of", n, "\r")
    utils::flush.console()
  }


  return(D)
}

#' @rdname score_cooksdistance
#' @export
scoreCooksDistance <- function(object, print=TRUE) {
  message("Please note that 'scoreCooksDistance()' is now deprecated, it is better to use 'score_cooksdistance()' instead.")
  score_cooksdistance(object, print)
}
