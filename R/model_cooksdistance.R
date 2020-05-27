#' @title Cook's distances
#'
#' @description  Calculates Cook's distances for each observation.
#' Please, note that it will work only for functions with specified \code{update} method.
#'
#' @param object An object of class \code{explainer} created with function \code{\link[DALEX]{explain}} from the DALEX package.
#'
#' @references Cook, R. Dennis (1977). "Detection of Influential Observations in Linear Regression". doi:10.2307/1268249.
#'
#' @return An object of the class \code{auditor_model_cooksdistance}.
#'
#' @examples
#' data(titanic_imputed, package = "DALEX")
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic_imputed)
#'
#' # use DALEX package to wrap up a model into explainer
#' glm_audit <- audit(model_glm,
#'                    data = titanic_imputed,
#'                    y = titanic_imputed$survived)
#'
#' # validate a model with auditor
#' mc <- model_cooksdistance(glm_audit)
#' mc
#'
#' plot(mc)
#'
#' @export
model_cooksdistance <- function(object){
  check_object(object, type = "exp")

  cooksDistances <- score_cooksdistance(object)

  result <- data.frame(cooksDistances, object$label, 1:length(object$y), stringsAsFactors = TRUE)
  colnames(result) <- c("_cooks_dist_", "_label_", "_index_")
  result <- result[order(result[,"_cooks_dist_"], decreasing = TRUE),]

  class(result) <- c("auditor_model_cooksdistance", "data.frame")

  return(result)
}


#' @rdname model_cooksdistance
#' @export
observationInfluence <- function(object){
  warning("Please note that 'observationInfluence()' is now deprecated, it is better to use 'model_cooksdistance()' instead.")
  model_cooksdistance(object)
}
