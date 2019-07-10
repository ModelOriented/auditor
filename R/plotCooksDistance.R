#' @title Influence of observations plot
#'
#' @description Cook’s distances are used for estimate the influence of an single observation.
#'
#'
#' @param object An object of class ModelAudit.
#' @param nlabel Number of observations with the biggest Cook's distances to be labeled.
#' @param ... Other arguments passed to \code{\link{scoreCooksDistance}}.
#'
#' @details Cook’s distance is a tool for identifying observations that may negatively affect the model.
#' They may be also used for indicating regions of the design space where it would be good to obtain more observations.
#' Data points indicated by Cook’s distances are worth checking for validity.
#'
#' Cook’s Distances are calculated by removing the i-th observation from the data and recalculating the model.
#' It shows how much all the values in the model change when the i-th observation is removed.
#'
#' For model classes other than lm and glm the distances are computed directly from the definition.
#'
#'
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' plotCooksDistance(lm_au)
#'
#' @import ggplot2
#'
#' @export
plotCooksDistance <- function(object, ..., nlabel = 3) {

  # some safeguard
  index <- cooks.dist <- big <- nameIndex <- variable <- label <- NULL

  # check if passed object is of class "observationInfluence" or "modelAudit"
  check_object(object, type = "infl")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., variable = variable, type = "infl", nlabel = nlabel)

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(length(levels(df$label))))

  # main chart
  p <- ggplot(data = df, aes(index, cooks.dist))

  # points
  p <- p + drwhy_geom_point(df, alpha_val = 0.95)
  p <- p + geom_point(data = subset(df, big == TRUE), aes(colour = label), size = 1.5)

  # text labels for extreme observations
  p <- p + geom_text_repel(data = subset(df, big == TRUE), aes(label = as.character(index)),
                           color = "#f05a71", size = 3)

  # theme, colours, titles, axes, scales, etc.
  p <- p +
    theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3")) +
    scale_color_manual(values = rev(colours), breaks = levels(df$label), guide = guide_legend(nrow = 1))

  p <- p + scale_x_continuous(breaks = scales::pretty_breaks())

  p + xlab("Observation index") + ylab("Cook's distance") + ggtitle("Influence of observations")

}
