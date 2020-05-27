#' @title Two-sided Cumulative Distribution Function
#'
#' @description Cumulative Distribution Function for positive and negative residuals.
#'
#' @param object An object of class 'auditor_model_residual' created with \code{\link{model_residual}} function.
#' @param ... Other modelAudit objects to be plotted together.
#' @param scale_error  A logical value indicating whether ECDF should be scaled by proportions of positive and negative proportions.
#' @param outliers Number of outliers to be marked.
#' @param residuals A logical value indicating whether residuals should be marked.
#' @param reverse_y A logical value indicating whether values on y axis should be reversed.
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
#' mr_lm <- model_residual(lm_audit)
#' plot_tsecdf(mr_lm)
#' plot(mr_lm, type="tsecdf")
#'
#' library(randomForest)
#' model_rf <- randomForest(life_length~., data = dragons)
#' rf_audit <- audit(model_rf, data = dragons, y = dragons$life_length)
#' mr_rf <- model_residual(rf_audit)
#' plot_tsecdf(mr_lm, mr_rf, reverse_y = TRUE)
#'
#'
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#'
#' @export
plot_tsecdf <- function(object, ..., scale_error = TRUE, outliers = NA,
                             residuals = TRUE, reverse_y = FALSE) {
  # some safeguard
  res <- ecd <- `_label_` <- big <- no.obs <- ord <- NULL

  # check if passed object is of class "auditor_model_residual"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., type = "ecdf", scale_error = scale_error, outliers = outliers,
                       reverse_y = reverse_y)

  # new varibale to set an order o curves
  df$ord <- paste(rev(as.numeric(factor(df$`_label_`))), df$`_label_`)

  # colors for model(s)
  colours <- theme_drwhy_colors(nlevels(df$`_label_`))

  # main chart
  p <- ggplot(df, aes(x = res, y = ecd, colour = `_label_`, group = ord)) +
    geom_step() +
    scale_colour_manual(values = colours, breaks = levels(df$`_label_`), guide = guide_legend(nrow = 1)) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(df$ecd) * 1.05), labels = scales::percent) +
    theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3"),
          plot.subtitle = element_text(vjust = -1)) +
    xlab("Residuals") +
    ylab("") +
    ggtitle("Two-sided cumulative distribution function", subtitle = " ")

  if (residuals == TRUE) {
    df <- df[order(-as.numeric(factor(df$`_label_`))), ]
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


#' @rdname plot_tsecdf
#' @export
plotTwoSidedECDF <- function(object, ..., scale_error = TRUE, outliers = NA,
                             residuals = TRUE, reverse_y = FALSE) {
  warning("Please note that 'plotTwosidedECDF()' is now deprecated, it is better to use 'plot_tsecdf()' instead.")
  plot_tsecdf(object, ..., scale_error, outliers, residuals, reverse_y)
}
