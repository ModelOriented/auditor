#' @title Predicted response vs Observed or Variable Values
#'
#' @description Plot of predicted response vs observed or variable Values.
#'
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param ... Other modelAudit or modelResiduals objects to be plotted together.
#' @param variable Only for modelAudit objects. Name of model variable to order residuals.
#' If value is NULL data order is taken. If value is "Observed response" the data is ordered
#' by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#' @param smooth Logical, indicates whenever smooth line should be added.
#' @param abline Logical, indicates whenever function y=x shoul be added.
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' plotPrediction(lm_au, variable = "prestige", abline = TRUE)
#'
#' library(randomForest)
#' rf_model <- randomForest(prestige~education + women + income, data = Prestige)
#' rf_au <- audit(rf_model, data = Prestige, y = Prestige$prestige)
#' plotPrediction(lm_au, rf_au, variable = "prestige", smooth = TRUE)
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#'
#' @export
plotPrediction <- function(object, ..., variable = NULL, smooth = FALSE, abline = FALSE) {

  if(!("modelResiduals" %in% class(object) || "modelAudit" %in% class(object))) {
    stop("The function requires an object created with audit() or modelResiduals().")
  }
  if("modelResiduals" %in% class(object)) variable <- object$variable[1]
  if(!("modelResiduals" %in% class(object))) object <- modelResiduals(object, variable)

  val <- fitted.values <- label <- NULL

  # data frame for the chart
  df <- object
  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if("modelAudit" %in% class(resp)) df <- rbind(df, modelResiduals(resp, variable) )
      if("modelResiduals" %in% class(resp)) df <- rbind(df, resp)
    }
  }

  # data for additional geoms
  maybe_smooth <- if (smooth == TRUE) df else df[0, ]

  # colors for model(s)
  colours <- theme_drwhy_colors(length(unique(df$label)))

  # main chart
  p <- ggplot(data = df, aes(val, fitted.values)) +
    geom_point(aes(color = label),
               alpha = ifelse(smooth == TRUE, 0.65, 1),
               stroke = 0) +
    scale_color_manual(values = colours) +
    theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3"))

  # abline, drawn only when response variable was passed
  if (abline == TRUE & !is.na(df$variable[1])) {
    p <- p + geom_abline(colour = "#ae2c87", alpha = 0.65)
  }

  # smoot curve(s)
  if (smooth == TRUE) {
    p <- p + geom_smooth(data = maybe_smooth,
                         aes(colour = factor(label, levels = rev(levels(maybe_smooth$label)))),
                         stat = "smooth",
                         method = "gam",
                         formula = y ~ s(x, bs = "cs"),
                         se = FALSE,
                         size = 1,
                         show.legend = TRUE)
  }

  # axes, scales, titles, etc.
  chart_title <- "Predicted"

  if (is.na(df$variable[1])) {
    variable <- "Observations"
    p <- p + scale_x_continuous(breaks = 5, labels = "")
  } else {
    chart_title <- paste0(chart_title, " vs ", variable)
    p <- p + scale_x_continuous(breaks = scales::pretty_breaks())
  }

  p <- p +
    xlab(variable) +
    ylab("Predicted values") +
    ggtitle(chart_title)

  return(p)
}
