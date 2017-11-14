#' @title ACF plot
#'
#' @description Plot autocorrelation function of model residuals.
#' Runs test, Durbin-Watson test
#'
#'
#' @param object An object of class ModelAudit
#'
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggplot2 ggtitle xlab ylab
#' @importFrom ggplot2 theme_classic
#' @importFrom forecast Acf ggAcf
#'
#' @export
plot_acf <- function(object){
  ggAcf(object$ordered.resid) +
    geom_point() +
    ggtitle("Autocorrelation Function") +
    theme_classic()
}
