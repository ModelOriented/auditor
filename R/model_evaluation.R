#' @title Create model evaluation explanation
#'
#' @description Creates explanation of classification model.
#'
#' Returns, among others, true positive rate (tpr), false positive rate (fpr),
#' rate of positive prediction (rpp), and true positives (tp).
#'
#' Created object of class \code{auditor_model_evaluation} can be used to plot
#' Receiver Operating Characteristic (ROC) curve (plot \code{\link{plot_roc}}) and LIFT curve (plot \code{\link{plot_lift}}).
#'
#' @param object An object of class \code{explainer} created with function \code{\link[DALEX]{explain}} from the DALEX package.
#'
#' @return An object of the class \code{auditor_model_evaluation}.
#'
#' @examples
#' data(titanic_imputed, package = "DALEX")
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic_imputed)
#'
#' glm_audit <- audit(model_glm,
#'                    data= titanic_imputed,
#'                    y = titanic_imputed$survived)
#'
#' # validate a model with auditor
#' me <- model_evaluation(glm_audit)
#' me
#'
#' plot(me)
#'
#' @export
model_evaluation <- function(object) {

  # checking if correct object is passed to the function
  check_object(object, type = "exp")

  df <- data.frame(y_hat = object$y_hat, y = object$y)
  df <- df[order(df$y_hat, decreasing = TRUE), ]

  roc_y <- factor(df$y)
  positive_label <- levels(roc_y)[2]
  negative_label <- levels(roc_y)[1]


  # true & false positives
  tp_duplicates <- cumsum(df$y == positive_label)
  fp_duplicates <- cumsum(df$y == negative_label)

  # remove duplicates
  duplicates <- rev(duplicated(rev(df$y_hat)))
  tp <- c(0, tp_duplicates[!duplicates])
  fp <- c(0, fp_duplicates[!duplicates])

  # number of positives & negatives
  n_pos <- sum(df$y == positive_label)
  n_neg <- sum(df$y == negative_label)

  # false negatives & true negatives
  fn <- n_pos - tp
  tn <- n_neg - fp

  # number of positive predistions & number of negative predictions
  n_pos_pred <- tp + fp
  n_neg_pred <- fn + tn

  # true positive rate & false positive rate
  tpr <- tp / n_pos
  fpr <- fp / n_neg

  # rate of positive predictions
  rpp <- (tp + fp) / (tp + fp + tn + fn)

  precision <- tp / (tp + fp)
  recall <- tp / n_pos


  # final data frame
  result <- data.frame(y_hat = c(0, object$y_hat[!duplicates]),
                       y = c(0, factor(object$y)[!duplicates]),
                       cutoffs = c(0, df$y_hat[!duplicates]),
                       tpr = tpr,
                       fpr = fpr,
                       rpp = rpp,
                       tp = tp,
                       precision = precision,
                       recall = recall,
                       label = factor(object$label), stringsAsFactors = TRUE)

  colnames(result) <- paste0("_", colnames(result), "_")
  class(result) <- c("auditor_model_evaluation", "data.frame")

  result
}



#' @rdname model_evaluation
#' @export
modelEvaluation <- function(object) {
  warning("Please note that 'modelEvaluation()' is now deprecated, it is better to use 'model_evaluation()' instead.")
  model_evaluation(object)
}
