#' @title Plot Residuals vs Observed, Fitted or Variable Values
#'
#' @description A plot of residuals against fitted values, observed values or any variable.
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param ... Other modelAudit objects to be plotted together.
#' @param variable Only for modelAudit object. Name of model variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#' @param points Logical, indicates whenever observations should be added as points.
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
plotResidual <- function(object, ..., variable = NULL, points = TRUE, smooth = FALSE,
                         std.residuals = FALSE, nlabel = 0) {

  if(!("modelResiduals" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelResiduals().")
  if("modelResiduals" %in% class(object)) variable <- object$variable[1]
  if(!("modelResiduals" %in% class(object))) object <- modelResiduals(object, variable)

  res <- std.res <- val <- label <- index <- NULL

  df <- object
  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if ("modelAudit" %in% class(resp)) df <- rbind(df, modelResiduals(resp, variable))
      if ("modelResiduals" %in% class(resp)) df <- rbind(df, resp)
    }
  }

  ytitle <- "Residuals"
  if (std.residuals == TRUE) {
    res <- std.res
    ytitle <- "Standardized residuals"
  }

  # data frames for each geom
  maybe_points <- if (points == TRUE) df else df[0, ]
  maybe_smooth  <- if (smooth  == TRUE) df else df[0, ]
  maybe_labels <- df[order(abs(df$res), decreasing = TRUE),][0:nlabel, ] # ?????

  # depending of how many models are presented (1 or more) - colors and other values are changing
  colours <- theme_drwhy_colors(length(unique(df$label)))

  p <- ggplot() +
    geom_point(data = df,
               aes(val, res, color = label),
               alpha = ifelse(smooth == TRUE, 0.65, 1),
               stroke = 0) +
    geom_smooth(data = maybe_smooth,
                aes(val, res, colour = factor(label, levels = rev(levels(maybe_smooth$label)))),
                stat = "smooth",
                method = "gam",
                formula = y ~ s(x, bs = "cs"),
                se = FALSE,
                size = 1,
                show.legend = TRUE) +
    scale_color_manual(values = colours) +
    theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3")) +
    geom_text_repel(data = maybe_labels,
                    aes(val, res, label = as.character(index)),
                    hjust = -0.2,
                    vjust = -0.2,
                    color = theme_drwhy_colors(1),
                    size = 3.3,
                    show.legend = FALSE)

  chart_title <- "Residuals"

  if (is.na(df$variable[1])) {
    variable <- "Observations"
    p <- p + scale_x_continuous(breaks = 5, labels = "")
  } else {
    chart_title <- paste0("Residuals vs ", variable)
    p <- p + scale_x_continuous(breaks = scales::pretty_breaks())
  }

  p <- p + xlab(variable) + ylab(ytitle) + ggtitle(chart_title)

  return(p)

}

