#' @title Create Model Residuals Explaination
#'
#' @description  Creates 'auditor_model_residual' that contains sorted residuals. An object can be further used to generate plots.
#' For the list of possible plots see see also section.
#'
#' @param object An object of class 'explainer' created with function \code{\link[explain]{DALEX}} from the DALEX package.
#' @param variable Name of model variable to order residuals.
#' If value is NULL the data is ordered by a vector of actual response (\code{y} parameter
#' passed to the \code{\link{explain}} function). One can also pass any name of any other variable
#' in the data set. If \code{variable = ""} is set, unordered observations are presented.
#'
#' @seealso \code{\link{plot_acf}, \link{plot_autocorrelation}, \link{plot_residual}, \link{plot_residual_boxplot},
#' \link{plot_pca}, \link{plot_correlation}, \link{plot_prediction}, \link{plot_rec}, \link{plot_residual_density},
#' \link{plot_residual}, \link{plot_rroc}, \link{plot_scalelocation}, \link{plot_tsecdf}}
#'
#' @examples
#' library(DALEX)
#' data(DALEX::titanic)
#' titanic <- na.omit(titanic)
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#' exp_glm <- explain(model_glm, data = titanic, y = titanic$survived)
#'
#' library(auditor)
#' model_residual(exp_glm)
#'
#' @rdname model_residual
#'
#' @export
model_residual <- function(x, variable = NULL){
  check_object(x, type = "exp")

  if (!is.null(variable)) {
    if (variable != "" & !variable %in% colnames(x$data)) {
      stop("The model_residual() function requires `variable = NULL`, `variable = ''` or the name of variable from model data frame.")
    }
  }

  ordered_df <- order_residuals(x, variable)

  if (is.null(variable)) {
    variable <- "Target variable"
  } else if (variable == "") {
    variable <- "Observations"
  }
  result <- data.frame(label = x$label,
                       res = ordered_df$residuals,
                       val = ordered_df$values,
                       variable = variable,
                       y = ordered_df$y,
                       fitted_values = ordered_df$y_hat,
                       std_res = ordered_df$std_residuals,
                       index = ordered_df$index
  )
  class(result) <- c("auditor_model_residual", "data.frame")

  return(result)
}


#' @rdname model_residual
#' @export
modelResiduals <- function(x, variable = NULL){
  message("Please note that 'modelResiduals()' is now deprecated, it is better to use 'model_residual()' instead.")
  model_residual(x, variable)
}
