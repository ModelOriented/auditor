#' @title Create Model Evaluation Explaination
#'
#' @description  Creates explanation of classification model.
#'
#' Returns, among others, true positive rate (tpr), false positive rate (fpr),
#' rate of positive prediction (rpp), and true positives (tp).
#'
#' Created object of class 'auditor_model_evaluation' can be used to plot
#' Receiver Operating Characteristic (ROC) curve (plot \code{\link{plot_roc}}) and LIFT curve (plot \code{\link{plot_lift}}).
#'
#' @param object An object of class 'explainer' created with function \code{\link[DALEX]{explain}} from the DALEX package.
#'
#' @return An object of class 'auditor_model_evaluation'.
#'
#' @examples
#' titanic <- na.omit(DALEX::titanic)
#' titanic$survived <- titanic$survived == "yes"
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#'
#' # use DALEX package to wrap up a model into explainer
#' exp_glm <- DALEX::explain(model_glm, data= titanic, y = titanic$survived)
#'
#' # validate a model with auditor
#' library(auditor)
#' model_evaluation(exp_glm)
#'
#'
#' @export
model_evaluation <- function(object){
  check_object(object, type = "exp")

  result <- calculate_classif_evaluation(object$y_hat, object$y, object$label)

    class(result) <- c("auditor_model_evaluation", "data.frame")
  return(result)
}



calculate_classif_evaluation <- function(predictions, y, label){

  y <- factor(y)
  levels <- levels(y)
  pos_label <- levels[2]
  neg_label <- levels[1]

  pred <- data.frame(predictions = predictions, y = y)

  pred_sorted <- pred[order(pred$predictions, decreasing = TRUE), ]
  # true positives & false negatives
  tp <- cumsum(pred_sorted$y == pos_label)
  fp <- cumsum(pred_sorted$y == neg_label)
  # cutoffs aka thresholds aka alpha
  cutoffs <- pred_sorted$predictions
  # number of positives & negatives
  n_pos <- sum(y == levels[2] )
  n_neg <- sum(y == levels[1] )
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
  rpp <- (tp + fp) / (tp +fp +tn +fn)
  res <- data.frame(fitted_values = predictions,
             y = y,
             cutoffs = cutoffs,
             tpr = tpr,
             fpr = fpr,
             rpp = rpp,
             tp = tp)
  colnames(res) <- c("_y_hat_", "_y_", "_cutoffs_", "_tpr_", "_fpr_", "_rpp_", "_tp_")
  res$`_label_` <- label
  res
}


#' @rdname model_evaluation
#' @export
modelEvaluation <- function(object) {
  message("Please note that 'modelEvaluation()' is now deprecated, it is better to use 'model_evaluation()' instead.")
  model_evaluation(object)
}
