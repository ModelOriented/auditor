#' @title Create Model Residuals explainer
#'
#' @description  Creates modelResiduals object to be plotted.
#'
#' @param object An object of class 'ModelAudit'explain' created with functio \code{\link[explain]{DALEX}} from the DALEX package.
#' @param variable Name of model variable to order residuals.
#' If value is NULL the data is ordered by a vector of actual response (\code{y} parameter
#' passed to the \code{\link{explain}} function). One can also pass any name of any other variable
#' in the data set. If \code{variable = ""} is set, unordered observations are presented.
#'
#' @examples
#' library(DALEX)
#' data(titanic)
#' titanic <- na.omit(titanic)
#'
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#'
#' exp_glm <- explain(model_glm, data = titanic, y = titanic$survived)
#'
#' library(auditor)
#' model_residual(exp_glm)
#'
#' @rdname model_residual
#'
#' @export
model_residual <- function(x, variable = NULL){
  if (is.null(x$data)) stop("The model_residual() function requires explainers created with specified 'data' parameter.")
  if (is.null(x$y)) stop("The model_residual() function requires explainers created with specified 'y' parameter.")

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
  class(result) <- c("model_residual", "data.frame")

  return(result)
}


#' @rdname model_residual
#' @export
modelResiduals <- function(x, variable = NULL){
  message("Please note that 'modelResiduals()' is now deprecated, it is better to use 'model_residual()' instead.")
  model_residual(x, variable)
}
