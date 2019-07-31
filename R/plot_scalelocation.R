#' @title Scale location plot
#'
#' @description Variable values vs square root of the absolute value of the residuals.
#' A vertical line corresponds to median.
#'
#'
#' @param object An object of class 'model_audit' or 'model_residual'.
#' @param ... Other modelAudit objects to be plotted together.
#'@param variable Name of variable to order residuals on a plot.
#' If \code{variable="_y_"}, the data is ordered by a vector of actual response (\code{y} parameter
#' passed to the \code{\link{explain}} function).
#' If \code{variable = "_y_hat_"} the data on the plot will be ordered by predicted response.
#' If \code{variable = NULL}, unordered observations are presented.
#' @param smooth Logical, indicates whenever smoothed lines should be added. By default it's FALSE.
#' @param peaks A logical value. If TRUE peaks are marked on plot by black dots.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_exp <- DALEX::explain(lm_model, data = dragons, y = dragons$life_length)
#' library(auditor)
#' lm_mr <- model_residual(lm_exp)
#' plot_scalelocation(lm_mr)
#'
#'
#' @import ggplot2
#' @importFrom stats median
#'
#' @export
plot_scalelocation <- function(object, ..., variable = "_y_", smooth = FALSE, peaks = FALSE) {

  # some safeguard
  values <- sqrt_std_residuals <- peak <- label <- maybe_peaks <- maybe_smooth <- NULL

  # check if passed object is of class "model_residuals" or "model_audit"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., variable = variable, type = "scal")

  if (is.null(variable)) {
    variable <- "Observations"
  }

  # set value for label of the X axis
  if (variable == "Observations") {
    x_lab <- "Observations"
  } else if (variable == "_y_")  {
    x_lab <- "Target variable"
  } else if (variable == "_y_hat_") {
    x_lab <- "Actual response"
  } else {
    x_lab <- as.character(df$`_variable_`[1])
  }

  # data frame for extra geoms
  maybe_peaks  <- if (peaks == TRUE) subset(df, peak == TRUE) else df[0, ]
  maybe_smooth <- if (smooth == TRUE) df else df[0, ]
  # colors for model(s)
  colours <- rev(theme_drwhy_colors(length(levels(df$label))))

  # main chart
  p <- ggplot(data = df, aes(`_val_`, `_sqrt_std_residuals_`))

  # scatter plot for the main model
  p <- p + drwhy_geom_point(df, smooth, alpha_val = 0.65)

  # smoot curve for the main model
  if (smooth == TRUE)
    p <- p + drwhy_geom_smooth(maybe_smooth)

  # points for peaks
  p <- p + geom_point(data = maybe_peaks, color = "#f05a71", shape = 4, size = 2, alpha = 1)

  # theme, colours, titles, axes, scales, etc.
  p <- p + theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3")) +
    scale_color_manual(values = rev(colours), breaks = levels(df$label), guide = guide_legend(nrow = 1))

  chart_title <- "Scale location"

  if ("model_audit" %in% class(object)) object <- model_residual(object, variable = variable)

  if (x_lab != "Observations") {
    p <- p + scale_x_continuous(breaks = scales::pretty_breaks())
    if (x_lab != "Target variable") chart_title <- paste0(chart_title, " vs ", x_lab)
  } else {
    p <- p + scale_x_continuous(breaks = 5, labels = "")
  }

  p <- p + xlab(x_lab) + ylab("\u221A|Standarized residuals|") + ggtitle(chart_title)

  return(p)
}


#' @rdname plot_scalelocation
#' @export
plotScaleLocation <- function(object, ..., variable = NULL, smooth = FALSE, peaks = FALSE) {
  message("Please note that 'plotScaleLocation()' is now deprecated, it is better to use 'plot_scalelocation()' instead.")
  plot_scalelocation(object, ..., variable, smooth, peaks)
}
