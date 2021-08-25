#' @title Area Under ROC Curve (AUC)
#'
#' @description Area Under Curve (AUC) for Receiver Operating Characteristic.
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
#' score_auc(glm_audit)
#'
#' @seealso \code{\link{plot_roc}}
#'
#' @export
score_auc <- function(object, data = NULL, y = NULL, ...) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  # use new data
  if (!is.null(data)){
    object$y <- y
    object$y_hat <- object$predict_function(object$model, data)
  }
  observed <- object$y
  predicted <- object$y_hat

  # code based on the solution from the DALEX package
  tpr_tmp <- tapply(observed, predicted, sum)
  TPR <- c(0,cumsum(rev(tpr_tmp)))/sum(observed)
  fpr_tmp <- tapply(1 - observed, predicted, sum)
  FPR <- c(0,cumsum(rev(fpr_tmp)))/sum(1 - observed)

  auc <- sum(diff(FPR)*(TPR[-1] + TPR[-length(TPR)])/2)

  roc_results <- list(
    name = "auc",
    score = auc
  )

  class(roc_results) <- "auditor_score"
  return(roc_results)
}

#' @title One minus Area Under ROC Curve (AUC)
#'
#' @description One minus Area Under Curve (AUC) for Receiver Operating Characteristic.
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
#' score_one_minus_auc(glm_audit)
#'
#' @export
score_one_minus_auc <- function(object, data = NULL, y = NULL, ...) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  ret <- 1 - score_auc(object, data, y, ...)$score
  roc_results <- list(
    name = "one_minus_auc",
    score = ret
  )

  class(roc_results) <- "auditor_score"
  return(roc_results)
}

#' @rdname score_auc
#' @export
scoreROC<- function(object) {
  warning("Please note that 'scoreROC()' is now deprecated, it is better to use 'score_auc()' instead.")
  score_auc(object)
}
