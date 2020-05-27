#' @title Autocorrelation Function Plot
#'
#' @description Plot Autocorrelation Function of models' residuals.
#'
#' @param object An object of class \code{auditor_model_residual} created with \code{\link{model_residual}} function.
#' @param ... Other \code{auditor_model_residual} objects to be plotted together.
#' @param variable Name of variable to order residuals on a plot.
#' If \code{variable="_y_"}, the data is ordered by a vector of actual response (\code{y} parameter
#' passed to the \code{\link[DALEX]{explain}} function).
#' If \code{variable = "_y_hat_"} the data on the plot will be ordered by predicted response.
#' If \code{variable = NULL}, unordered observations are presented.
#' @param alpha Confidence level of the interval.
#'
#' @return A ggplot object.
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
#' mr_lm <- model_residual(lm_audit)
#'
#' # plot results
#' plot(mr_lm, type = "acf")
#' plot_acf(mr_lm)
#'
#'
#' library(randomForest)
#' model_rf <- randomForest(life_length~., data = dragons)
#' rf_audit <- audit(model_rf, data = dragons, y = dragons$life_length)
#' mr_rf <- model_residual(rf_audit)
#' plot_acf(mr_lm, mr_rf)
#' plot(mr_lm, mr_rf, type="acf")
#'
#'
#' @import ggplot2
#' @importFrom stats qnorm acf
#'
#' @export
plot_acf <- function(object, ..., variable = NULL, alpha = 0.95) {
  # some safeguard
  lag <- acf <- ymin <- NULL

  # check if passed object is of class "model_residual" or "model_audit"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., variable = variable, type = "res")

  result_df <- data.frame(acf = numeric(), label = character(), lag = numeric(), ymin = numeric())
  for (label in unique(df$`_label_`)) {
    orderedResiduals <- df[which(df$`_label_` == label), "_residuals_"]
    acf <- acf(orderedResiduals, plot = FALSE)
    result_df <- rbind(result_df, data.frame(acf = acf$acf[-1], label = label, lag = acf$lag[-1], ymin = 0))
  }

  df <- result_df

  # set value for label of the X axis
  if (is.null(variable)) {
    x_lab <- "Observations"
  } else if (variable == "_y_")  {
    x_lab <- "target variable"
  } else if (variable == "_y_hat_") {
    x_lab <- "actual response"
  } else {
    x_lab <- as.character(variable)
  }

  conf_lims <- c(-1, 1) * qnorm((1 + alpha) / 2) / sqrt(nrow(object))

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(nlevels(df$label)))

  p <- ggplot(df, aes(x = lag)) +
    geom_segment(aes(x = lag, xend = lag, y = ymin, yend = acf, colour = label), size = 1, alpha = 0.65) +
    geom_hline(yintercept = conf_lims, color = "darkgrey", linetype = "dashed") +
    facet_wrap(. ~ label, scales = "free_y", ncol = 1)

  # theme, colours, titles, axes, scales, etc.
  p <- p + theme_drwhy() +
    theme(axis.line.x = element_blank(),
          strip.text = element_text(margin = margin(t = 10)),
          panel.spacing = unit(1, "lines"),
          legend.text = element_text(margin = margin(r = 5, l = 3)),
          legend.key = element_rect(colour = NA, fill = NA),
          legend.position = "none") +
    scale_color_manual(values = rev(colours), breaks = levels(result_df$label), guide = guide_legend(nrow = 1))

  p <- p + scale_x_continuous(breaks = scales::pretty_breaks())

  if (x_lab != "Observations") x_lab <- paste0("Lag by ", x_lab)

  p + xlab(x_lab) + ylab("") + ggtitle("ACF plot")

}

#' @rdname plot_acf
#' @export
plotACF <- function(object, ..., variable = NULL, alpha = 0.95) {
  warning("Please note that 'plotACF()' is now deprecated, it is better to use 'plot_acf()' instead.")
  plot_acf(object, ..., variable = variable, alpha = alpha)
}
