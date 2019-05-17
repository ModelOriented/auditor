#' @title Create Model Residuals explainer
#'
#' @description  Creates modelResiduals object to be plotted.
#'
#' @param object An object of class ModelAudit.
#' @param variable Optional. Name of variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#'
#' @examples
#' library(MASS)
#' model.glm <- glm(Postwt ~ Prewt + Treat + offset(Prewt), family = gaussian, data = anorexia)
#' audit.glm <- audit(model.glm)
#'
#' mr.glm <- modelResiduals(audit.glm)
#'
#' @export
modelResiduals <- function(object, variable = NULL){
  if(!("modelAudit" %in% class(object))) stop("The function requires an object created with audit().")
  if (!is.null(variable)) {
    if (variable != "" & !variable %in% colnames(object$data)) {
      stop("The function requires `variable = NULL`, `variable = ''` or the name of variable from model data frame.")
    }
  }

  residuals <- orderResidualsDF(object, variable, is.df = TRUE)
  std.residuals <- orderResidualsDF(object, variable, type = "std.residuals")
  y <- orderResidualsDF(object, variable, type = "y")
  fitted.values <- orderResidualsDF(object, variable, type = "fitted.values")
  if (is.null(variable)) {
    variable <- "Target variable"
  } else if (variable == "") {
    variable <- "Observations"
  }

  result <- data.frame(label = object$label,
                       res = residuals$residuals,
                       val = residuals$value,
                       variable = variable,
                       y = y,
                       fitted.values = fitted.values,
                       std.res = std.residuals,
                       index = residuals$index
  )
  class(result) <- c("modelResiduals", "data.frame")

  return(result)
}
