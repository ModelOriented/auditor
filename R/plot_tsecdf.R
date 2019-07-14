#' @title Two-sided Cumulative Distribution Function
#'
#' @description Cumulative Distribution Function for positive and negative residuals.
#'
#' @param object An object of class ModelAudit.
#' @param ... Other modelAudit objects to be plotted together.
#' @param error.scaled  A logical value indicating whether ECDF should be scaled by proportions of positive and negative proportions.
#' @param outliers Number of outliers to be marked.
#' @param residuals A logical value indicating whether residuals should be marked.
#' @param y.reversed A logical value indicating whether values on y axis should be reversed.
#'
#' @return ggplot object
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' plotTwoSidedECDF(lm_au)
#'
#' library(randomForest)
#' rf_model <- randomForest(life_length~., data = dragons)
#' rf_au <- audit(rf_model, data = dragons, y = dragons$life_length)
#' plotTwoSidedECDF(lm_au, rf_au, y.reversed = TRUE)
#'
#' @seealso \code{\link{plot.model_audit}}
#'
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#'
#' @export
plotTwoSidedECDF <- function(object, ..., error.scaled = TRUE, outliers = NA,
                             residuals = TRUE, y.reversed = FALSE) {
  # some safeguard
  res <- ecd <- label <- big <- no.obs <- NULL

  # check if passed object is of class "modelResiduals" or "modelAudit"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., type = "ecdf", error.scaled = error.scaled, outliers = outliers,
                       y.reversed = y.reversed)

  # new varibale to set an order o curves
  df <- df[order(-as.numeric(factor(df$ecd))), ]

  # colors for model(s)
  colours <- theme_drwhy_colors(length(levels(df$label)))

  # main chart
  p <- ggplot(df, aes(x = res, y = ecd, colour = label, group = label)) +
    geom_step() +
    scale_colour_manual(values = colours, breaks = levels(df$label), guide = guide_legend(nrow = 1)) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(df$ecd) * 1.05), labels = scales::percent) +
    theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3")) +
    xlab("Residuals") +
    ylab("What ?") +
    ggtitle("Two-sided Cumulative Distribution Function")

  if (residuals == TRUE) {
    df <- df[order(-as.numeric(factor(df$label))), ]
    p + geom_point(data = df, show.legend = FALSE, size = 1) +
      geom_text_repel(data = subset(df, big == TRUE),
                      aes(label = as.character(no.obs)),
                      color = "#160e3b",
                      segment.colour = "#160e3b",
                      show.legend = FALSE,
                      direction = "y",
                      nudge_x = 0.7,
                      size = 3.5)
  } else {
    p
  }
}
