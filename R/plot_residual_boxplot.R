#' @title Plot Boxplots of Residuals
#'
#' @description A plot of residuals.
#'
#' @param object An object of class modelAudit.
#' @param ... Other modelAudit objects to be plotted together.
#'
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' plotResidual(lm_au)
#'
#' library(randomForest)
#' rf_model <- randomForest(life_length~., data = dragons)
#' rf_au <- audit(rf_model, data = dragons, y = dragons$life_length)
#' plotResidualBoxplot(lm_au, rf_au)
#'
#' @seealso \code{\link{plot.model_audit}}
#'
#' @import ggplot2
#' @importFrom stats aggregate
#'
#' @export
plotResidualBoxplot <- function(object, ...) {

  # some safeguard
  res <- label <- NULL

  # check if passed object is of class "modelResiduals" or "modelAudit"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., type = "res")

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(length(levels(df$label))))

  # additional values
  df_points <- aggregate(list(res = df$res), list(label = df$label), FUN = function(x) { sqrt(mean(x^2)) })

  # main chart
  ggplot(data = df, aes(x = label, y = abs(res), fill = label)) +
    geom_boxplot(coef = 1000, show.legend = FALSE, width = 0.65) +
    geom_point(data = df_points, aes(x = label, y = res), shape = 4, size = 2.5, show.legend = FALSE) +
    xlab("") +
    ylab("") +
    ggtitle("Absolute residuals") +
    theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3"), panel.grid = element_blank()) +
    scale_fill_manual(values = rev(colours), breaks = levels(df$label)) +
    coord_flip()
}


