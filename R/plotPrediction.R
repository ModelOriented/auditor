#' @title Predicted response vs Observed or Variable Values
#'
#' @description Plot of predicted response vs observed or variable Values.
#'
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param ... Other modelAudit or modelResiduals objects to be plotted together.
#' @param variable Only for modelAudit objects. Name of model variable to order residuals.
#' If value is NULL the data is ordered by a vector of actual response (\code{y} parameter
#' passed to the \code{\link{audit}} function). One can also pass any name of any other variable
#' in the data set. If \code{variable = ""} is set, unordered observations are presented.
#' @param smooth Logical, indicates whenever smooth line should be added.
#' @param abline Logical, indicates whenever function y = x shoul be added. Works only
#' with \code{variable = NULL} which is a default option.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' plotPrediction(lm_au, variable = "height", abline = TRUE)
#'
#' library(randomForest)
#' rf_model <- randomForest(life_length~., data = dragons)
#' rf_au <- audit(rf_model, data = dragons, y = dragons$life_length)
#' plotPrediction(lm_au, rf_au, variable = "height", smooth = TRUE)
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#'
#' @export
plotPrediction <- function(object, ..., variable = NULL, smooth = FALSE, abline = FALSE) {

  # some safeguard
  val <- fitted.values <- label <- NULL

  # check if passed object is of class "modelResiduals" or "modelAudit"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., variable = variable, type = "res")

  # data frame for extra geoms
  maybe_smooth <- if (smooth == TRUE) df else df[0, ]

  # set value for label of the X axis
  x_lab <- as.character(df$variable[1])

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(length(levels(df$label))))

  # main chart
  p <- ggplot(data = df, aes(val, fitted.values))

  # scatter plot for the main model
  p <- p + drwhy_geom_point(df, smooth, alpha_val = 0.65)

  # smoot curve for the main model
  if (smooth == TRUE)
    p <- p + drwhy_geom_smooth(maybe_smooth)

  # abline, drawn only when response variable was passed
  if (abline == TRUE & x_lab == "Target variable")
    p <- p + geom_abline(colour = "#ae2c87", alpha = 0.65)

  # theme, colours, titles, axes, scales, etc.
  p <- p + theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3")) +
    scale_color_manual(values = rev(colours), breaks = levels(df$label), guide = guide_legend(nrow = 1))

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
