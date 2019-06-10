#' @title Autocorrelation Function Plot
#'
#' @description Plot Autocorrelation Function of models residuals.
#'
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param ... Other modelAudit or modelResiduals objects to be plotted together.
#' @param variable Only for modelAudit object. Name of model variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#' @param alpha Confidence level of the interval.
#'
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' plotACF(lm_au)
#'
#' library(randomForest)
#' rf_model <- randomForest(prestige~education + women + income, data = Prestige)
#' rf_au <- audit(rf_model, data = Prestige, y = Prestige$prestige)
#' plotACF(lm_au, rf_au)
#'
#'
#' @import ggplot2
#' @importFrom stats qnorm acf
#'
#' @export
plotACF <- function(object, ..., variable=NULL, alpha = 0.95){
  # some safeguard
  lag <- acf <- ymin <- NULL

  # check if passed object is of class "modelResiduals" or "modelAudit"
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
    facet_grid(label ~ ., switch = "y", scales = "free_y")


  # theme, colours, titles, axes, scales, etc.
  p <- p + theme_drwhy() +
    theme(axis.line.x = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank()) +
    scale_color_manual(values = rev(colours),
                       breaks = levels(df$label))

  p <- p + scale_x_continuous(breaks = scales::pretty_breaks())

  p + xlab("Lag") + ylab("") + ggtitle("ACF plot")



}
