#' @title Autocorrelation Function Plot
#'
#' @description Plot Autocorrelation Function of models residuals.
#'
#'
#' @param object An object of class 'auditor_model_residual' created with \code{\link{model_residual}} function.
#' @param ... Other 'auditor_model_residual' objects to be plotted together. Please, note that they should be created with the same value
#' of parameter 'variable'.
#' @param alpha Confidence level of the interval.
#'
#' @return A ggplot object.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_exp <- DALEX::explain(lm_model, data = dragons, y = dragons$life_length)
#' library(auditor)
#' plot_acf(lm_exp)
#'
#' library(randomForest)
#' rf_model <- randomForest(life_length~., data = dragons)
#' rf_exp <- explain(rf_model, data = dragons, y = dragons$life_length)
#' plot_acf(lm_exp, rf_exp)
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
  for (label in unique(df$label)) {
    orderedResiduals <- df[which(df$label == label), "res"]
    acf <- acf(orderedResiduals, plot = FALSE)
    resultDF <- rbind(resultDF, data.frame(acf = acf$acf[-1], label = label, lag = acf$lag[-1], ymin = 0))
  }

  conf_lims <- c(-1, 1) * qnorm((1 + alpha) / 2) / sqrt(nrow(df))

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(length(levels(df$label))))

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
    scale_color_manual(values = rev(colours), breaks = levels(df$label), guide = guide_legend(nrow = 1))

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
