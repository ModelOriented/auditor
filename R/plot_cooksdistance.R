#' @title Influence of Observations Plot
#'
#' @description Plot of Cook’s distances used for estimate the influence of an single observation.
#'
#' @param object An object of class \code{auditor_model_cooksdistance} created with \code{\link{model_cooksdistance}} function.
#' @param ... Other objects of class \code{auditor_model_cooksdistance}.
#' @param nlabel Number of observations with the biggest Cook's distances to be labeled.
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
#' @references Cook, R. Dennis (1977). "Detection of Influential Observations in Linear Regression". doi:10.2307/1268249.
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
#' library(auditor)
#' cd_lm <- model_cooksdistance(lm_audit)
#'
#' # plot results
#' plot_cooksdistance(cd_lm)
#' plot(cd_lm, type = "cooksdistance")
#'
#' @import ggplot2
#'
#' @export
plot_cooksdistance <- function(object, ..., nlabel = 3) {

  # some safeguard
  '_index_' <- '_cooks_dist_' <- '_big_' <- '_label_' <- NULL

  # check if passed object is of class "observationInfluence" or "modelAudit"
  check_object(object, type = "infl")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., type = "infl", nlabel = nlabel)

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(nlevels(df$`_label_`)))
  # main chart
  p <- ggplot(data = df, aes(`_index_`, `_cooks_dist_`))

  # points
  p <- p + drwhy_geom_point(df, alpha_val = 0.95)
  p <- p + geom_point(data = subset(df, `_big_` == TRUE), aes(colour = `_label_`), size = 1.5)

  # text labels for extreme observations
  p <- p + geom_text_repel(data = subset(df, `_big_` == TRUE), aes(label = as.character(`_index_`)),
                           color = "#f05a71", size = 3)

  # theme, colours, titles, axes, scales, etc.
  p <- p +
    theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3")) +
    scale_color_manual(values = rev(colours), breaks = levels(df$`_label_`), guide = guide_legend(nrow = 1))

  p <- p + scale_x_continuous(breaks = scales::pretty_breaks())

  p + xlab("Observation index") + ylab("Cook's distance") + ggtitle("Influence of observations")


}

#' @rdname plot_cooksdistance
#' @export
plotCooksDistance <- function(object, ..., nlabel = 3) {
  warning("Please note that 'plotCookDistance()' is now deprecated, it is better to use 'plot_cooksdistance()' instead.")
  plot_cooksdistance(object, ..., nlabel = nlabel)
}
