#' @title Predicted response vs Observed or Variable Values
#'
#' @description Plot of predicted response vs observed or variable Values.
#'
#'
#' @param object An object of class 'model_audit' or 'model_residual.
#' @param ... Other modelAudit or modelResiduals objects to be plotted together.
#' @param variable Name of variable to order residuals on a plot.
#' If \code{variable="_y_"}, the data is ordered by a vector of actual response (\code{y} parameter
#' passed to the \code{\link{explain}} function).
#' If \code{variable = "_y_hat_"} the data on the plot will be ordered by predicted response.
#' If \code{variable = NULL}, unordered observations are presented.
#' @param smooth Logical, indicates whenever smooth line should be added.
#' @param abline Logical, indicates whenever function y = x shoul be added. Works only
#' with \code{variable = NULL} which is a default option.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_exp <- DALEX::explain(lm_model, data = dragons, y = dragons$life_length)
#'
#' library(auditor)
#' lm_mr <- model_residual(lm_exp)
#' plot_prediction(lm_mr, variable = "height", abline = TRUE)
#'
#' library(randomForest)
#' rf_model <- randomForest(life_length~., data = dragons)
#' rf_exp <- DALEX::explain(rf_model, data = dragons, y = dragons$life_length)
#' rf_mr <- model_residual(rf_exp)
#' plot_prediction(lm_mr, rf_mr, variable = "height", smooth = TRUE)
#'
#'
#' @import ggplot2
#'
#' @export
plot_prediction <- function(object, ..., variable = "_y_", smooth = FALSE, abline = FALSE) {
  # some safeguard
  val <- fitted_values <- label <- NULL

  # check if passed object is of class "model_residual" or "model_audit"
  check_object(object, type = "res")


  # data frame for ggplot object
  df <- make_dataframe(object, ..., variable = variable, type = "res")

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
  maybe_smooth <- if (smooth == TRUE) df else df[0, ]


  # colors for model(s)
  colours <- rev(theme_drwhy_colors(length(levels(df$`_label_`))))
  # main chart
  p <- ggplot(data = df, aes(`_val_`, `_y_hat_`))

  # scatter plot for the main model
  p <- p + drwhy_geom_point(df, smooth, alpha_val = 0.65)


  p# smoot curve for the main model
  if (smooth == TRUE)
    p <- p + drwhy_geom_smooth(maybe_smooth)

  # abline, drawn only when response variable was passed
  if (abline == TRUE & x_lab == "Target variable")
    p <- p + geom_abline(colour = "#ae2c87", alpha = 0.65)

  # theme, colours, titles, axes, scales, etc.
  p <- p + theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3")) +
    scale_color_manual(values = rev(colours), breaks = levels(df$`_label_`), guide = guide_legend(nrow = 1))

  chart_title <- "Predicted"

  if (x_lab != "Observations") {
    p <- p + scale_x_continuous(breaks = scales::pretty_breaks())
    if (x_lab != "Target variable") chart_title <- paste0(chart_title, " vs ", x_lab)
  } else {
    p <- p + scale_x_continuous(breaks = 5, labels = "")
  }

  p <- p + xlab(x_lab) + ylab("Predicted values") + ggtitle(chart_title)

  return(p)
}

#' @rdname plot_prediction
#' @export
plotPrediction <- function(object, ..., variable = NULL, smooth = FALSE, abline = FALSE) {
  message("Please note that 'plotPrediction()' is now deprecated, it is better to use 'plot_prediction()' instead.")
  plot_prediction(object, ..., variable, smooth, abline)
}
