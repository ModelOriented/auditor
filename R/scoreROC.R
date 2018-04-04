#' @title Area Under Curve (AUC)
#'
#' @description Area Under Curve (AUC) for Receiver Operating Characteristic
#' @param object An object of class ModelAudit
#'
#' @return numeric
#'
#' @seealso \code{\link{plotROC}}
#'
#' @import pROC
#'
#' @export


scoreROC <- function(object, type="AUC"){

  auc(object$y, object$fitted.values)[1]
}
