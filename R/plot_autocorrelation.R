#' @title Autocorrelation of Residuals Plot
#'
#' @description Plot of i-th residual vs i+1-th residual.
#'
#' @param object An object of class 'auditor_model_residual' created with \code{\link{model_residual}} function.
#' @param ... Other 'auditor_model_residual' objects to be plotted together.
#' @param variable Name of variable to order residuals on a plot.
#' If \code{variable="_y_"}, the data is ordered by a vector of actual response (\code{y} parameter
#' passed to the \code{\link[DALEX]{explain}} function).
#' @param smooth Logical, if TRUE smooth line will be added.
#'
#' @return A ggplot object.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#'
#' # fit a model
#' lm_model <- lm(life_length ~ ., data = dragons)
#'
#' # use DALEX package to wrap up a model into explainer
#' lm_exp <- DALEX::explain(lm_model, data = dragons, y = dragons$life_length)
#'
#' # validate a model with auditor
#' library(auditor)
#' lm_mr <- model_residual(lm_exp)
#'
#' # plot results
#' plot_autocorrelation(lm_mr)
#' plot(lm_mr, type = "autocorrelation")
#' plot_autocorrelation(lm_mr, smooth = TRUE)
#' plot(lm_mr, type = "autocorrelation", smooth = TRUE)
#'
#' @import ggplot2
#'
#' @export
plot_autocorrelation <- function(object, ..., variable = "_y_hat_", smooth = FALSE) {

  # some safeguard
  x <- y <- x_val <- y_val <- NULL

  # check if passed object is of class "model_residuals" or "model_audit"
  check_object(object, type = "res")

  # data frame for ggplot object
  df_temp <- make_dataframe(object, ..., variable = variable, type = "res")

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


  df <- data.frame(x_val = numeric(), y_val = numeric(), label = character())
  for (label in levels(df_temp$`_label_`)) {
    ord_res <- df_temp[which(df_temp$`_label_` == label), "_residuals_"]
    df <- rbind(df, data.frame(x_val = ord_res[-length(ord_res)],
                               y_val = ord_res[-1],
                               label = label))
  }
  colnames(df)[3] <- "_label_"

  # data frame for extra geoms
  maybe_smooth <- if (smooth == TRUE) df else df[0, ]

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(length(levels(df$label))))

  # main chart
  p <- ggplot(df, aes(x_val, y_val))

  # scatter plot for the main model
  p <- p + drwhy_geom_point(df, smooth, alpha_val = 0.65)

  # smoot curve for the main model
  if (smooth == TRUE)
    p <- p + drwhy_geom_smooth(maybe_smooth)

  # theme, colours, titles, axes, scales, etc.
  p <- p + theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3"),
          plot.subtitle = element_text(vjust = -1)) +
    scale_color_manual(values = rev(colours), breaks = levels(df$label), guide = guide_legend(nrow = 1))

  chart_title <- "Autocorrelation "


  if (x_lab == "Target variable") {
    p <- p + scale_x_continuous(breaks = scales::pretty_breaks())
    chart_subtitle <- paste0("of residuals ordered by predicted values")
  } else if (x_lab == "Actual response") {
    p <- p + scale_x_continuous(breaks = scales::pretty_breaks())
    chart_subtitle <- paste0("of residuals ordered by model response")
  } else if (x_lab == "Observations") {
    p <- p + scale_x_continuous(breaks = 5, labels = "")
    chart_subtitle <- paste0("of unordered residuals")
  } else {
    p <- p + scale_x_continuous(breaks = scales::pretty_breaks())
    chart_subtitle <- paste0("of residuals ordered by ", x_lab)
}

p <- p + xlab("Residual i") + ylab("Residual i+1") + ggtitle(chart_title, subtitle = chart_subtitle)

return(p)
}


#' @rdname plot_autocorrelation
#' @export
plotAutocorrelation <- function(object, ..., variable, smooth = FALSE) {
  message("Please note that 'plotAutocorrelation()' is now deprecated, it is better to use 'plot_autocorrelation()' instead.")
  plot_autocorrelation(object, ..., smooth)
}
