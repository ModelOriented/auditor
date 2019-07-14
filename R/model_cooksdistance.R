#' @title Create Observation Influence Explainer
#'
#' @description  Calculates Cook's distances.
#'
#' @param object An object of class model_audit.
#' @param ... other parameters.
#'
#' @examples
#' library(DALEX)
#' data(titanic)
#' titanic <- na.omit(titanic)
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#' audit_glm <- audit(model_glm)
#'
#' model_cooksdistance(audit_glm)
#'
#' @export
model_cooksdistance <- function(object, ...){

  cooksDistances <- scoreCooksDistance(object, ...)

  result <- data.frame(cooks.dist = cooksDistances, label = object$label, index = 1:length(object$y))
  result <- result[order(-result$cooks.dist),]

  class(result) <- c("observationInfluence", "data.frame")

  return(result)
}


#' @rdname model_cooksdistance
#' @export
observationInfluence <- function(object, ...){
  message("Please note that 'observationInfluence()' is now deprecated, it is better to use 'model_cooksdistance()' instead.")
  model_cooksdistance(object, ...)
}
