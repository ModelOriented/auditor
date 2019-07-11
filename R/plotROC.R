#' @title Receiver Operating Characteristic (ROC)
#'
#' @description Receiver Operating Characterstic Curve is a plot of the true positive rate (TPR) against the false positive rate (FPR) for the different thresholds.
#' It is useful for measuring and comparing the accuracy of the classificators.
#'
#' @param object An object of class ModelAudit or modelEvaluation.
#' @param ... Other modelAudit objects to be plotted together.
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}, \link{plotRROC}, \link{plotREC}}
#'
#' @import ggplot2
#'
#' @examples
#' library(DALEX)
#' data(titanic)
#' titanic <- na.omit(titanic)
#' titanic$survived <- titanic$survived == "yes"
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#' audit_glm <- audit(model_glm, y = titanic$survived)
#'
#' plotROC(audit_glm)
#'
#' @export


plotROC <- function(object, ...){
  if(!("modelEvaluation" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelEvaluation().")
  if(!("modelEvaluation" %in% class(object))) object <- modelEvaluation(object)
  label <- fpr <- tpr <- NULL

  df <- make_dataframe(object, ..., type = "eva")

  ggplot(df, aes(x = fpr, y = tpr, color = label)) +
    geom_step() +
    xlab("false positive fraction") +
    ylab("true positive fraction") +
    ggtitle("ROC Curve") +
    theme_light()
}

