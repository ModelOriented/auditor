#' @title ACF plot
#'
#' @description Plot autocorrelation function of model residuals.
#' Runs test, Durbin-Watson test
#'
#'
#' @param object An object of class ModelAudit
#' @param variable name of dependent or independent variable to order residuals. If NULL the fitted values are taken.
#'
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggplot2 ggtitle xlab ylab
#' @importFrom ggplot2 theme_classic
#' @importFrom forecast Acf ggAcf
#'
#' @export
plotACF <- function(object, variable=NULL){
  if(is.null(variable) || variable=="Fitted values") variable <- "Fitted values"
  orderedResiduals <- getOrderedResiduals(object, variable)

  ggAcf(orderedResiduals) +
    geom_point() +
    ggtitle("Autocorrelation Function") +
    theme_classic()
}

getOrderedResiduals <- function(object, variable){
  if(variable == "Fitted values") {
    values <- object$fitted.values
  } else {
    values <- object$data[,variable]
  }
  tmpDF <- data.frame(values = values, residuals=object$residuals)
  orderedResiduals <- dplyr::arrange(tmpDF, values)$residuals
  return(orderedResiduals)
}
