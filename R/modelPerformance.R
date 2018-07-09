#' @title Create Model Performance explainer
#'
#' @details Creates model Performance object to be plotted.
#'
#' @param object An object of class ModelAudit.
#' @param variable Optional. Name of variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#'
#' @export
modelPerformance <- function(object, variable = NULL){
    residuals <- orderResidualsDF(object, variable, is.df = TRUE)
    std.residuals <- orderResidualsDF(object, variable, type = "std.residuals")
    y <- orderResidualsDF(object, variable, type = "y")
    fitted.values <- orderResidualsDF(object, variable, type = "fitted.values")
    if(is.null(variable)) variable <- NA

  data.frame(label = object$label,
             res = residuals$residuals,
             val = residuals$value,
             variable = variable,
             y = y,
             fitted.values = fitted.values,
             std.res = std.residuals
             )
}
