#' @title Autocorrelation Function Plot
#'
#' @description Plot Autocorrelation Function of models residuals.
#'
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param ... Other modelAudit or modelResiduals objects to be plotted together.
#' @param variable Only for model_audit object. Name of model variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#' @param alpha Confidence level of the interval.
#'
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' plotACF(lm_au)
#'
#' library(randomForest)
#' rf_model <- randomForest(life_length~., data = dragons)
#' rf_au <- audit(rf_model, data = dragons, y = dragons$life_length)
#' plot_acf(lm_au, rf_au)
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
