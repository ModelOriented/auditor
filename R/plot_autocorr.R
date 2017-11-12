#' @title Autocorrelation plot
#'
#' @description Plot i-th residual vs i+1-th residual. Checking autocorrelation of residuals.
#' Runs test, Durbin-Watson test
#'
#'
#' @param object An object of class ModelAudit
#'
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggplot2 geom_point geom_hline
#' @importFrom ggplot2 ggtitle xlab ylab
#' @importFrom ggplot2 theme_classic
#'
#' @export
plot_autocorr <- function(object){
  x <- y <- NULL

  resid <- object$ordered.resid
  n <- length(resid)
  df <- data.frame(x = resid[-n], y = resid[-1])

  ggplot(df, aes(x, y)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    xlab("residual i") +
    ylab("residual i+1") +
    ggtitle("Autocorrelation") +
    theme_classic()
}
