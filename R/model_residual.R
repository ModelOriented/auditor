#' @title Create Model Residuals Explanation
#'
#' @description  Creates \code{auditor_model_residual} that contains sorted residuals.
#'  An object can be further used to generate plots.
#' For the list of possible plots see see also section.
#'
#' @param object An object of class \code{explainer} created with function \code{\link[DALEX]{explain}} from the DALEX package.
#' @param ... other parameters
#'
#' @seealso \code{\link{plot_acf}, \link{plot_autocorrelation}, \link{plot_residual}, \link{plot_residual_boxplot},
#' \link{plot_pca}, \link{plot_correlation}, \link{plot_prediction}, \link{plot_rec}, \link{plot_residual_density},
#' \link{plot_residual}, \link{plot_rroc}, \link{plot_scalelocation}, \link{plot_tsecdf}}
#'
#' @return An object of the class \code{auditor_model_residual}.
#'
#' @examples
#' library(DALEX)
#'
#' # fit a model
#' model_glm <- glm(m2.price ~ ., data = apartments)
#'
#' glm_audit <- explain(model_glm,
#'                    data = apartments,
#'                    y = apartments$m2.price)
#'
#' # validate a model with auditor
#' mr <- model_residual(glm_audit)
#' mr
#'
#' plot(mr)
#'
#' @rdname model_residual
#'
#' @export
model_residual <- function(object, ...){
  check_object(object, type = "exp")

  result <- data.frame(matrix(nrow=length(object$residuals), ncol = 0))

  result$`_residuals_` = object$residuals
  result$`_std_residuals_` <- object$residuals / sd(object$residuals)
  result$`_y_` <- object$y
  result$`_y_hat_` <- object$y_hat
  result$`_index_` <- rownames(object$data)
  result$`_label_` <- factor(object$label)

  result <- cbind(result, object$data)

  class(result) <- c("auditor_model_residual", "data.frame")

  return(result)
}


#' @rdname model_residual
#' @export
modelResiduals <- function(object, ...){
  warning("Please note that 'modelResiduals()' is now deprecated, it is better to use 'model_residual()' instead.")
  model_residual(object)
}
