#' @title ACF plot
#'
#' @description Plot autocorrelation function of model residuals.
#' Runs test, Durbin-Watson test
#'
#'
#' @param object An object of class ModelAudit
#' @param variable name of variable to order residuals
#'
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggplot2 ggtitle xlab ylab
#' @importFrom ggplot2 theme_classic
#' @importFrom forecast Acf ggAcf
#'
#' @export
plotACF <- function(object, variable){
  modelData <- object$data
  modelData$residuals <- object$residuals
  orderedResiduals <- arrange_(modelData, variable)$residuals

  ggAcf(orderedResiduals) +
    geom_point() +
    ggtitle("Autocorrelation Function") +
    theme_classic()
}
