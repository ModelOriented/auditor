#' @title Plot Autocorrelation Function in D3 with r2d3 package.
#'
#' @description Plot Autocorrelation Function of models residuals.
#'
#' @param object An object of class 'model_audit' or 'model_residuals'.
#' @param ... Other 'model_audit' or modelResiduals objects to be plotted together.
#' @param variable Only for 'model_audit' object. Name of model variable to order residuals.
#' If value is NULL data order is taken.
#' If value is "Predicted response" or "Fitted values" then data is ordered by fitted values.
#' If value is "Observed response" the data is ordered by a vector of actual response
#' @param alpha Confidence level of the interval.
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's FALSE.
#'
#' @return a `r2d3` object.
#'
#'
#' @importFrom stats qnorm acf
#'
#' @export
#' @rdname plotD3_acf

plotD3_acf <- function(object, ..., variable = NULL, alpha = 0.95, scale_plot = FALSE) {

  # some safeguard
  lag <- acf_df <- ymin <- NULL

  x_title <- ifelse(!is.null(variable) && nchar(variable) > 1, paste0("Lag of ", variable), "")
  chart_title <- "ACF plot"

  n <- length(list(object, ...))

  # check if passed object is of class "model_residual" or "model_audit"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., variable = variable, type = "res")

  result_df <- data.frame(acf = numeric(), label = character(), lag = numeric(), ymin = numeric())
  for (label in unique(df$`_label_`)) {
    ordered_residuals <- df[which(df$`_label_` == label), "_residuals_"]
    acf_df <- acf(ordered_residuals, plot = FALSE)
    result_df <- rbind(result_df, data.frame(acf = acf_df$acf[-1],
                                             label = label,
                                             lag = acf_df$lag[-1],
                                             ymin = 0))
  }

  conf_lims <- c(-1, 1) * qnorm((1 + alpha) / 2) / sqrt(nrow(object))

  xmax <- max(result_df$lag)+1
  xmin <- min(result_df$lag)-1
  ymax <- max(result_df$acf, conf_lims, result_df$ymin)
  ymin <- min(result_df$acf, conf_lims, result_df$ymin)

  y_margin <- (abs(ymax-ymin))*0.05

  line_data <- split(result_df, f = result_df$label)

  temp <- jsonlite::toJSON(list(line_data, conf_lims))

  options <- list(xmax = xmax, xmin = xmin,
                  ymax = ymax + y_margin, ymin = ymin - y_margin,
                  scalePlot = scale_plot, n = n,
                  xTitle = x_title, chartTitle = chart_title)

  r2d3::r2d3(data = temp, script = system.file("d3js/plotACFMany.js", package = "auditor"),
             dependencies = list(
               system.file("d3js/colorsDrWhy.js", package = "auditor"),
               system.file("d3js/tooltipD3.js", package = "auditor")
             ),
             css = system.file("d3js/themeDrWhy.css", package = "auditor"),
             d3_version = 4,
             options = options)
}


#' @rdname plotD3_acf
#' @export
plotD3ACF <- function(object, ..., variable = NULL, alpha = 0.95, scale_plot = FALSE) {
  message("Please note that 'plotD3ACF()' is now deprecated, it is better to use 'plotD3_acf()' instead.")
  plotD3_acf(object, ..., variable, alpha, scale_plot)
}
