#' @title Gini Coefficient
#'
#' @description The Gini coefficient measures the inequality among values of a frequency distribution.
#' A Gini coefficient equals 0 means perfect equality, where all values are the same.
#' A Gini coefficient equals 100% expresses maximal inequality of values.
#'
#' @param object An object of class \code{explainer} created with function
#'  \code{\link[DALEX]{explain}} from the DALEX package.
#' @param data New data that will be used to calculate the score.
#'  Pass \code{NULL} if you want to use \code{data} from \code{object}.
#' @param y New y parameter will be used to calculate score.
#' @param ... Other arguments dependent on the type of score.
#'
#' @return An object of class \code{auditor_score}.
#'
#' @examples
#' library(DALEX)
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic_imputed)
#'
#' # create an explainer
#' exp_glm <- explain(model_glm,
#'                    data = titanic_imputed,
#'                    y = titanic_imputed$survived)
#'
#' # calculate score
#' score_gini(exp_glm)
#'
#' @seealso \code{\link{plot_roc}}
#'
#' @export
score_gini <- function(object, data = NULL, y = NULL, ...) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  # inject new data to the explainer
  if (!is.null(data)){
    object$data <- data
    object$y <- y
    object$y_hat <- object$predict_function(object$model, data)
  }

  auc <- score_auc(object, data)$score
  gini <- 2 * auc - 1

  results <- list(
    name = "gini",
    score = gini
  )

  class(results) <- "auditor_score"
  return(results)
}

#' @title One minus Gini Coefficient
#'
#' @description One minus Gini Coefficient
#' 100% means perfect equality, where all values are the same.
#' 0 expresses maximal inequality of values.
#'
#' @param object An object of class \code{explainer} created with function
#'  \code{\link[DALEX]{explain}} from the DALEX package.
#' @param data New data that will be used to calculate the score.
#'  Pass \code{NULL} if you want to use \code{data} from \code{object}.
#' @param y New y parameter will be used to calculate score.
#' @param ... Other arguments dependent on the type of score.
#'
#' @return An object of class \code{auditor_score}.
#'
#' @examples
#' data(titanic_imputed, package = "DALEX")
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic_imputed)
#'
#' glm_audit <- audit(model_glm,
#'                    data = titanic_imputed,
#'                    y = titanic_imputed$survived)
#'
#' # calculate score
#' score_one_minus_gini(glm_audit)
#'
#'
#' @export
score_one_minus_gini <- function(object, data = NULL, y = NULL, ...) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  # inject new data to the explainer
  if (!is.null(data)){
    object$data <- data
    object$y <- y
    object$y_hat <- object$predict_function(object$model, data)
  }

  ret <- 1 - score_gini(object)$score
  results <- list(
    name = "one_minus_gini",
    score = ret
  )

  class(results) <- "auditor_score"
  return(results)
}
