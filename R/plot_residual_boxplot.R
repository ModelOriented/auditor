#' @title Plot Boxplots of Residuals
#'
#' @description A boxplot of residuals.
#'
#' @param object An object of class 'auditor_model_residual' created with \code{\link{model_residual}} function.
#' @param ... Other 'auditor_model_residual' objects to be plotted together.
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
#' plot_residual_boxplot(mr_lm)
#' plot(mr_lm, type = "residual_boxplot")
#'
#' library(randomForest)
#' model_rf <- randomForest(life_length~., data = dragons)
#' exp_rf <- DALEX::explain(model_rf, data = dragons, y = dragons$life_length)
#' mr_rf <- model_residual(exp_rf)
#' plot_residual_boxplot(mr_lm, mr_rf)
#' plot(mr_lm, mr_rf)
#'
#' @seealso \code{\link{plot.model_audit}}
#'
#' @rdname plot_residual_boxplot
#'
#' @import ggplot2
#' @importFrom stats aggregate
#'
#' @export
plot_residual_boxplot <- function(object, ...) {

  # some safeguard
  `_residuals_` <- `_label_` <- label <- res <- NULL

  # check if passed object is of class "model_residual" or "model_audit"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., type = "res")

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(nlevels(df$`_label_`)))

  # additional values
  df_points <- aggregate(list(res = df$`_residuals_`), list(label = df$`_label_`), FUN = function(x) { sqrt(mean(x^2)) })
  # main chart
  ggplot(data = df, aes(x = `_label_`, y = abs(`_residuals_`), fill = label)) +
    geom_boxplot(coef = 1000, show.legend = FALSE, width = 0.65) +
    geom_point(data = df_points, aes(x = label, y = res), shape = 4, size = 2.5, show.legend = FALSE) +
    xlab("") +
    ylab("") +
    ggtitle("Absolute residuals") +
    theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3"), panel.grid = element_blank()) +
    scale_fill_manual(values = rev(colours), breaks = levels(df$`_label_`)) +
    coord_flip()
}

#' @rdname plot_residual_boxplot
#' @export
plotResidualBoxplot <- function(object, ...) {
  message("Please note that 'plotResidualBoxplot()' is now deprecated, it is better to use 'plot_residual_boxplot()' instead.")
  plot_residual_boxplot(object, ...)
}
