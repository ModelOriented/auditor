#' @title Residuals plot
#'
#' @description Residuals vs fitted values or dependent or independent variable
#'
#'
#' @param object An object of class ModelAudit
#' @param variable name of dependent or independent variable to order residuals. If NULL the fitted values are taken.
#'
#'
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggplot2 geom_point geom_hline
#' @importFrom ggplot2 ggtitle xlab ylab
#' @importFrom ggplot2 theme_classic
#'
#' @export
plotResiduals <- function(object, variable=NULL){
  residuals <- values <- NULL
  if(is.null(variable)) variable <- "Fitted values"
  plotData <- generateResidualsDF(object, variable)

  ggplot(plotData, aes(values, residuals)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE) +
    xlab(variable) +
    ylab("Residuals") +
    ggtitle(paste0("Residuals vs ", variable)) +
    theme_classic()
}


generateResidualsDF <- function(object, variable){
  if(variable == "Fitted values") {
    values <- object$fitted.values
  } else {
    values <- object$data[,variable]
  }
  n <- length(object$residuals)
  resultDF <- data.frame(values = values, residuals = object$residuals)
  resultDF <- dplyr::arrange(resultDF, values)
  return(resultDF)
}
