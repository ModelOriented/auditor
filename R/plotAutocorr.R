#' @title Autocorrelation plot
#'
#' @description Plot i-th residual vs i+1-th residual. Checking autocorrelation of residuals.
#' Runs test, Durbin-Watson test
#'
#'
#' @param object An object of class ModelAudit
#' @param variable name of variable to order residuals
#'
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggplot2 geom_point geom_hline
#' @importFrom ggplot2 ggtitle xlab ylab
#' @importFrom ggplot2 theme_classic
#'
#' @export
plotAutocorr <- function(object, variable){
  x <- y <- NULL
  modelData <- object$data
  modelData$residuals <- object$residuals
  orderedResiduals <- arrange_(modelData, variable)$residuals

  n <- length(orderedResiduals)
  df <- data.frame(x = orderedResiduals[-n], y = orderedResiduals[-1])

  ggplot(df, aes(x, y)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    xlab("residual i") +
    ylab("residual i+1") +
    ggtitle("Autocorrelation") +
    theme_classic()
}
