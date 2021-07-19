#' @title Accuracy
#'
#' @param object An object of class \code{explainer} created with function
#'  \code{\link[DALEX]{explain}} from the DALEX package.
#' @param cutoff Threshold value, which divides model predicted values (y_hat) to calculate confusion matrix.
#'  By default it's \code{0.5}.
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
#' score_acc(glm_audit)
#'
#'
#' @export
#' @rdname score_acc
score_acc <- function(object, cutoff = 0.5, data = NULL, y = NULL, ...) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with audit() function.")

  # inject new data to the explainer
  if (!is.null(data)){
    object$data <- data
    object$y <- y
    object$y_hat <- object$predict_function(object$model, data)
  }

  conf <- confusionmatrix(object, cutoff)
  ret <- (conf$TP + conf$TN) / (conf$TP + conf$FP + conf$TN + conf$FN)

  acc_results <- list(
    name = "acc",
    score = ret
  )

  class(acc_results) <- "auditor_score"
  return(acc_results)
}


#' @title One minus accuracy
#'
#' @param object An object of class \code{explainer} created with function
#'  \code{\link[DALEX]{explain}} from the DALEX package.
#' @param cutoff Threshold value, which divides model predicted values to calculate confusion matrix.
#'  By default it's \code{0.5}.
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
#' # create an explainer
#' glm_audit <- audit(model_glm,
#'                    data = titanic_imputed,
#'                    y = titanic_imputed$survived)
#'
#' # calculate score
#' score_one_minus_acc(glm_audit)
#'
#'
#' @export
#' @rdname score_one_minus_acc
score_one_minus_acc <- function(object, cutoff = 0.5, data = NULL, y = NULL, ...) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with audit() function.")

  # inject new data to the explainer
  if (!is.null(data)){
    object$data <- data
    object$y <- y
    object$y_hat <- object$predict_function(object$model, data)
  }

  conf <- confusionmatrix(object, cutoff)
  ret <- 1 - (conf$TP + conf$TN) / (conf$TP + conf$FP + conf$TN + conf$FN)

  acc_results <- list(
    name = "one_minus_acc",
    score = ret
  )

  class(acc_results) <- "auditor_score"
  return(acc_results)
}
