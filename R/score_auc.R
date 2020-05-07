#' @title Area Under ROC Curve (AUC)
#'
#' @description Area Under Curve (AUC) for Receiver Operating Characteristic.
#'
#' @param object An object of class \code{explainer} created with function
#'  \code{\link[DALEX]{explain}} from the DALEX package.
#' @param data New data that will be used to calcuate the score.
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
#' score_auc(exp_glm)
#'
#' @seealso \code{\link{plot_roc}}
#'
#' @export
score_auc <- function(object, data = NULL, y = NULL, ...) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  # inject new data to the explainer
  if (!is.null(data)){
    object$data <- data
    object$y <- y
  }

  object <- model_evaluation(object)
  pred <- data.frame(y_hat = object$`_y_hat_`,
                     y = object$`_y_`)
  pred_sorted <- pred[order(pred$y_hat, decreasing = TRUE), ]
  roc_y <- factor(pred_sorted$y)

  positive_label <- levels(roc_y)[2]
  negative_label <- levels(roc_y)[1]

  positive_num <- sum(pred_sorted$y == positive_label)
  negative_num <- sum(pred_sorted$y == negative_label)

  tp <- cumsum(pred_sorted==positive_label)
  fp <- cumsum(pred_sorted==negative_label)

  # remove duplicates
  duplicates <- rev(duplicated(rev(pred_sorted$y_hat)))
  tp <- c(0, tp[!duplicates])
  fp <- c(0, fp[!duplicates])
  cutoffs <- c(Inf, pred_sorted$y_hat[!duplicates])

  x <- fp / negative_num
  y <- tp / positive_num

  auc <- sum( 0.5* (x[2:length(x)]-x[1:length(x)-1])* (y[2:length(x)] +y[1:length(x)-1]) )

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
#' @param data New data that will be used to calcuate the score.
#'  Pass \code{NULL} if you want to use \code{data} from \code{object}.
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
#' score_one_minus_auc(exp_glm)
#'
#' @export
score_one_minus_auc <- function(object, data = NULL, ...) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  # inject new data to the explainer
  if (!is.null(data)) object$data <- data

  ret <- 1 - score_auc(object)$score
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
