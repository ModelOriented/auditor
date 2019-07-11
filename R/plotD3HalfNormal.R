#' @title Plot Half-Normal in D3 with r2d3 package.
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
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's FALSE.
#'
#' @return a `r2d3` object.
#'
#' @importFrom hnp hnp
#' @importFrom fdrtool phalfnorm
#' @importFrom stats ecdf dnorm density
#'
#' @seealso \code{\link{scoreHalfNormal}, \link{plotHalfNormal}}
#'
#' @export
#' @rdname plotD3HalfNormal

plotD3HalfNormal <- function(object, ..., quantiles = FALSE, sim = 99, scale_plot = FALSE) {

  # some safeguard
  x <- residuals <- upper <- lower <- NULL

  # check if passed object is of class "modelFit" or "modelAudit"
  check_object(object, type = "fit")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., quant = quantiles, type = "fit")

  df
}
