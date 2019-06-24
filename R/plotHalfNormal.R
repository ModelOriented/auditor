#' @title Half-Normal plot
#'
#' @description The half-normal plot is one of the tools designed to evaluate the goodness of fit of a
#' statistical models. It is a graphical method for comparing two probability distributions by plotting
#' their quantiles against each other.
#' Points on the plot correspond to ordered absolute values of model diagnostic
#' (i.e. standardized residuals) plotted against theoretical order statistics from a half-normal distribution.
#'
#' @param object modelAudit object, modelFit object.
#' @param quantiles if TRUE values on axis are on quantile scale.
#' @param ... extra arguments passed to \link[hnp]{hnp}.
#' @param sim number of residuals to simulate
#'
#' @return An object of class ggplot
#'
#' @import ggplot2
#' @importFrom hnp hnp
#' @importFrom fdrtool phalfnorm
#' @importFrom stats ecdf dnorm density
#'
#' @seealso \code{\link{scoreHalfNormal}}
#'
#' @export

plotHalfNormal <- function(object, ..., quantiles = FALSE, sim = 99) {

  # some safeguard
  x <- residuals <- upper <- lower <- NULL

  # check if passed object is of class "modelFit" or "modelAudit"
  check_object(object, type = "fit")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., quant = quantiles, type = "fit")

  # main chart
  p <- ggplot(data = df, aes(x)) +
    geom_point(aes(y = residuals), colour = "#371ea3") +
    geom_line(aes(y = upper)) +
    geom_line(aes(y = lower)) +
    geom_line(aes(y = median), linetype = 2, colour = "darkgrey")

  # theme, colours, titles, axes, scales, etc.
  p <- p + theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3")) +
    xlab("Half-normal Quantiles") +
    ggtitle("Half-normal plot")

  if (quantiles == TRUE) {
    p + scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks()) +
      scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks()) +
      coord_fixed(ratio = 1) +
      ylab("Quantiles of |residuals|")
  } else {
    p + scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks()) +
      scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks()) +
      ylab("|Residuals|")
  }
}
