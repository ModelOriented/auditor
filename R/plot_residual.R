#' @title Plot Residuals vs Observed, Fitted or Variable Values
#'
#' @description A plot of residuals against fitted values, observed values or any variable.
#'
#' @param object An object of class 'auditor_model_residual' created with \code{\link{model_residual}} function.
#' @param ... Other 'auditor_model_residual' objects to be plotted together.
#' @param variable Name of variable to order residuals on a plot.
#' If \code{variable="_y_"}, the data is ordered by a vector of actual response (\code{y} parameter
#' passed to the \code{\link[DALEX]{explain}} function).
#' If \code{variable = "_y_hat_"} the data on the plot will be ordered by predicted response.
#' If \code{variable = NULL}, unordered observations are presented.
#' @param smooth Logical, indicates whenever smoothed lines should be added. By default it's FALSE.
#' @param std_residuals Logical, indicates whenever standardized residuals should be used.
#' @param nlabel Number of observations with the biggest Cook's distances to be labeled.
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
#' plot_residual(lm_mr)
#' plot(lm_mr, type = "residual")
#'
#' library(randomForest)
#' rf_model <- randomForest(life_length~., data = dragons)
#' rf_exp <- DALEX::explain(rf_model, data = dragons, y = dragons$life_length)
#' rf_mr <- model_residual(rf_exp)
#' plot_residual(lm_mr, rf_mr)
#' plot(lm_mr, rf_mr, type = "residual")
#'
#'
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#'
#' @export
plot_residual <- function(object, ..., variable = "_y_", smooth = FALSE,
                         std_residuals = FALSE, nlabel = 0) {

  # some safeguard
  res <- std_res <- val <- label <- index <- maybe_smooth <- maybe_labels <- NULL

  # check if passed object is of class "modelResiduals" or "modelAudit"
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
  maybe_labels <- df[order(abs(df$`_residuals_`), decreasing = TRUE),][0:nlabel, ]

  # set values for labels of axes
  y_lab <- "Residuals"
  if (std_residuals == TRUE) {
    df$res <- df$`_std_residuals_`
    y_lab <- "Standardized residuals"
  }

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(length(levels(df$`_label_`))))

  # main chart
  p <- ggplot(data = df, aes(`_val_`, `_residuals_`))

  # scatter plot for the main model
  p <- p + drwhy_geom_point(df, smooth, alpha_val = 0.65)

  # smoot curve for the main model
  if (smooth == TRUE)
    p <- p + drwhy_geom_smooth(maybe_smooth)

  if (nlabel > 0)
    p <- p + geom_text_repel(data = maybe_labels,
                             aes(label = as.character(index)),
                             hjust = -0.2,
                             vjust = -0.2,
                             color = theme_drwhy_colors(1),
                             size = 3.3,
                             show.legend = FALSE)

  # theme, colours, titles, axes, scales, etc.
  p <- p +
    theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3")) +
    scale_color_manual(values = rev(colours), breaks = levels(df$label), guide = guide_legend(nrow = 1))

  chart_title <- "Residuals"
  if (x_lab != "Observations") {
    p <- p + scale_x_continuous(breaks = scales::pretty_breaks())
    if (x_lab != "Target variable") chart_title <- paste0(chart_title, " vs ", x_lab)
  } else {
    p <- p + scale_x_continuous(breaks = 5, labels = "")
  }

  p <- p + xlab(x_lab) + ylab(y_lab) + ggtitle(chart_title)

  return(p)
}


#' @rdname plot_residual
#' @export
plotResidual <-  function(object, ..., variable = NULL, smooth = FALSE,
                           std_residuals = FALSE, nlabel = 0) {
  message("Please note that 'plotACF()' is now deprecated, it is better to use 'plot_acf()' instead.")
  plot_residuals(object, ..., variable, smooth, std_residuals, nlabel)
}
