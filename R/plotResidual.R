#' @title Plot Residuals vs Observed, Fitted or Variable Values
#'
#' @description A plot of residuals against fitted values, observed values or any variable.
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param ... Other modelAudit objects to be plotted together.
#' @param variable Only for modelAudit objects. Name of model variable to order residuals.
#' If value is NULL the data is ordered by a vector of actual response (\code{y} parameter
#' passed to the \code{\link{audit}} function). One can also pass any name of any other variable
#' in the data set. If \code{variable = ""} is set, unordered observations are presented.
#' @param smooth Logical, indicates whenever smoothed lines should be added. By default it's FALSE.
#' @param std.residuals Logical, indicates whenever standardized residuals should be used.
#' @param nlabel Number of observations with the biggest Cook's distances to be labeled.
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' plotResidual(lm_au)
#'
#' library(randomForest)
#' rf_model <- randomForest(prestige~education + women + income, data = Prestige)
#' rf_au <- audit(rf_model, data = Prestige, y = Prestige$prestige)
#' plotResidual(lm_au, rf_au)
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#'
#' @export
plotResidual <- function(object, ..., variable = NULL, smooth = FALSE,
                         std.residuals = FALSE, nlabel = 0) {

  # some safeguard
  res <- std.res <- val <- label <- index <- maybe_smooth <- maybe_labels <- NULL

  # check if passed object is of class "modelResiduals" or "modelAudit"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., variable = variable, type = "res")

  # data frame for extra geoms
  maybe_smooth <- if (smooth == TRUE) df else df[0, ]
  maybe_labels <- df[order(abs(df$res), decreasing = TRUE),][0:nlabel, ]

  # set values for labels of axes
  x_lab <- as.character(df$variable[1])
  y_lab <- "Residuals"
  if (std.residuals == TRUE) {
    df$res <- df$std.res
    y_lab <- "Standardized residuals"
  }

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(length(levels(df$label))))

  # main chart
  p <- ggplot(data = df, aes(val, res))

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
