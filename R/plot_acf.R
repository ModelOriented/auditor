#' @title Autocorrelation Function Plot
#'
#' @description Plot Autocorrelation Function of models' residuals.
#'
#' @param object An object of class 'auditor_model_residual' created with \code{\link{model_residual}} function.
#' @param ... Other 'auditor_model_residual' objects to be plotted together.
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
#' # use DALEX package to wrap up a model into explainer
#' exp_lm <- DALEX::explain(model_lm, data = dragons, y = dragons$life_length)
#'
#' # validate a model with auditor
#' library(auditor)
#' mr_lm <- model_residual(exp_lm)
#'
#' # plot results
#' plot(mr_lm, type = "acf")
#' plot_acf(mr_lm)
#'
#'
#' library(randomForest)
#' model_rf <- randomForest(life_length~., data = dragons)
#' exp_rf <- DALEX::explain(model_rf, data = dragons, y = dragons$life_length)
#' mr_rf <- model_residual(exp_rf)
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

  resultDF <- data.frame(acf = numeric(), label = character(), lag = numeric(), ymin = numeric())
  for (label in unique(df$`_label_`)) {
    orderedResiduals <- df[which(df$`_label_` == label), "_residuals_"]
    acf <- acf(orderedResiduals, plot = FALSE)
    resultDF <- rbind(resultDF, data.frame(acf = acf$acf[-1], label = label, lag = acf$lag[-1], ymin = 0))
  }

  conf_lims <- c(-1, 1) * qnorm((1 + alpha) / 2) / sqrt(nrow(object))
  # colors for model(s)
  colours <- rev(theme_drwhy_colors(nlevels(df$`_label_`)))

  p <- ggplot(resultDF, aes(x = lag)) +
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
    scale_color_manual(values = rev(colours), breaks = levels(df$`_label_`), guide = guide_legend(nrow = 1))

  p <- p + scale_x_continuous(breaks = scales::pretty_breaks())

  p + xlab(ifelse(!is.null(variable) & nchar(variable) > 1, paste0("Lag of ", variable))) +
    ylab("") + ggtitle("ACF plot")

}

#' @rdname plot_acf
#' @export
plotACF <- function(object, ..., variable = NULL, alpha = 0.95) {
  message("Please note that 'plotACF()' is now deprecated, it is better to use 'plot_acf()' instead.")
  plot_acf(object, ..., variable, alpha)
}
