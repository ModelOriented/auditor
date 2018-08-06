#' @title Create Observation Influence Explainer
#'
#' @description  Creates observationInfluence object to be plotted.
#'
#' @param object An object of class ModelAudit.
#' @param ... other parameters.
#'
#' @export
observationInfluence <- function(object, ...){


  cooksDistances <- scoreCooksDistance(object, ...)

  result <- data.frame(cooks.dist = cooksDistances, label = object$label, index = 1:length(object$y))
  result <- result[order(-result$cooks.dist),]

  class(result) <- c("observationInfluence", "data.frame")

  return(result)
}
