#' @title ACF plot
#'
#' @description Plot autocorrelation function of model residuals.
#' Runs test, Durbin-Watson test
#'
#'
#' @param object An object of class ModelAudit
#' @param variable name of dependent or independent variable to order residuals. If NULL the order from data is taken. If "Fitted values" then data is ordered by fitted values.
#'
#' @import ggplot2
#' @importFrom forecast Acf ggAcf
#'
#' @export
plotACF <- function(object, variable=NULL){
  orderedResiduals <- getOrderedResiduals(object, variable)

  ggAcf(orderedResiduals) +
    geom_point() +
    ggtitle("Autocorrelation Function") +
    theme_light()
}

getOrderedResiduals <- function(object, variable){

  if(is.null(variable)){
    values <- seq(1, nrow(object$data))
  } else {
    if(variable == "Fitted values") {
      values <- object$fitted.values
    } else {
      values <- object$data[,variable]
    }
  }

  tmpDF <- data.frame(values = values, residuals=object$residuals)
  orderedResiduals <- dplyr::arrange(tmpDF, values)$residuals
  return(orderedResiduals)
}
