#' @title Cook's distances
#'
#' @description  Calculates Cook's distances for each observation.
#' Please, note that it will work only for functions with specified 'update' method.
#'
#' @param object An object of class 'explain' created with function \code{\link[DALEX]{explain}} from the DALEX package.
#'
#' @references Cook, R. Dennis (1977). "Detection of Influential Observations in Linear Regression". doi:10.2307/1268249.
#'
#' @return An object of class 'auditor_model_cooksdistance'.
#'
#' @examples
#' titanic <- na.omit(DALEX::titanic)
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#'
#' # use DALEX package to wrap up a model into explainer
#' exp_glm <- DALEX::explain(model_glm, data = titanic, y = titanic$survived)
#'
#' # validate a model with auditor
#' library(auditor)
#' model_cooksdistance(exp_glm)
#'
#' @export
model_cooksdistance <- function(object){
  check_object(object, type = "exp")

  cooksDistances <- score_cooksdistance(object)

  result <- data.frame(cooksDistances, object$label, 1:length(object$y))
  colnames(result) <- c("_cooks_dist_", "_label_", "_index_")
  result <- result[order(result[,"_cooks_dist_"], decreasing = TRUE),]

  class(result) <- c("auditor_model_cooksdistance", "data.frame")

  return(result)
}


#' @rdname model_cooksdistance
#' @export
observationInfluence <- function(object){
  message("Please note that 'observationInfluence()' is now deprecated, it is better to use 'model_cooksdistance()' instead.")
  model_cooksdistance(object)
}
