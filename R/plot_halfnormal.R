#' @title Half-Normal plot
#'
#' @description The half-normal plot is one of the tools designed to evaluate the goodness of fit of a
#' statistical models. It is a graphical method for comparing two probability distributions by plotting
#' their quantiles against each other.
#' Points on the plot correspond to ordered absolute values of model diagnostic
#' (i.e. standardized residuals) plotted against theoretical order statistics from a half-normal distribution.
#'
#' @param object An object of class \code{auditor_model_halfnormal} created with \code{\link{model_halfnormal}} function.
#' @param ... Other \code{auditor_model_halfnormal} objects.
#' @param quantiles If TRUE values on axis are on quantile scale.
#' @param sim Number of residuals to simulate.
#'
#' @return A ggplot object.
#'
#' @seealso \code{\link{model_halfnormal}}
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#'
#' # fit a model
#' model_lm <- lm(life_length ~ ., data = dragons)
#'
#' lm_audit <- audit(model_lm, data = dragons, y = dragons$life_length)
#'
#' # validate a model with auditor
#' hn_lm <- model_halfnormal(lm_audit)
#'
#' # plot results
#' plot_halfnormal(hn_lm)
#' plot(hn_lm)
#'
#' @import ggplot2
#' @importFrom stats ecdf dnorm density
#'
#' @seealso \code{\link{score_halfnormal}}
#'
#' @export
plot_halfnormal <- function(object, ..., quantiles = FALSE, sim = 99) {

  # some safeguard
  `_x_` <- `_residuals_` <-`_upper_` <- `_lower_` <- `_median_` <- NULL

  # check if passed object is of class "auditor_model_halfnormal"
  check_object(object, type = "fit")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., quant = quantiles, type = "fit")

  # main chart
  p <- ggplot(data = df, aes(`_x_`)) +
    geom_point(aes(y = `_residuals_`), colour = "#371ea3") +
    geom_line(aes(y = `_upper_`)) +
    geom_line(aes(y = `_lower_`)) +
    geom_line(aes(y = `_median_`), linetype = 2, colour = "darkgrey")

  # theme, colours, titles, axes, scales, etc.
  p <- p + theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3")) +
    xlab("Half-normal Quantiles") +
    ggtitle("Half-normal plot")

  if (quantiles == TRUE) {
    p + scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks()) +
      scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks()) +
      ylab("Quantiles of |residuals|")
  } else {
    p + scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks()) +
      scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks()) +
      ylab("|Residuals|")
  }
}

#' @rdname plot_halfnormal
#' @export
plotHalfNormal <- function(object, ..., quantiles = FALSE, sim = 99) {
  warning("Please note that 'plotHalfNormal()' is now deprecated, it is better to use 'plot_halfnormal()' instead.")
  plot_halfnormal(object, ..., quantiles = quantiles, sim = sim)
}
