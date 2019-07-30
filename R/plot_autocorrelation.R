#' @title Autocorrelation of Residuals Plot
#'
#' @description Plot of i-th residual vs i+1-th residual.
#'
#' @param object An object of class 'auditor_model_residual' created with \code{\link{model_residual}} function.
#' @param ... Other 'auditor_model_residual' objects to be plotted together.
#' @param smooth Logical, if TRUE smooth line will be added.
#'
#' @return A ggplot object.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_exp <- DALEX::explain(lm_model, data = dragons, y = dragons$life_length)
#'
#' library(auditor)
#' plot_autocorrelation(lm_exp)
#' plot_autocorrelation(lm_exp, score = TRUE, smooth = TRUE)
#'
#' @import ggplot2
#'
#' @export
plot_autocorrelation <- function(object, ..., smooth = FALSE) {

  # some safeguard
  x <- y <- x_val <- y_val <- NULL

  # check if passed object is of class "model_residuals" or "model_audit"
  check_object(object, type = "res")

  # data frame for ggplot object
  df_temp <- make_dataframe(object, ..., variable = variable, type = "res")

  df <- data.frame(x_val = numeric(), y_val = numeric(), label = character())
  for (label in levels(df_temp$label)) {
    ord_res <- df_temp[which(df_temp$label == label), "res"]
    df <- rbind(df, data.frame(x_val = ord_res[-length(ord_res)],
                               y_val = ord_res[-1],
                               label = label))
  }

  # data frame for extra geoms
  maybe_smooth <- if (smooth == TRUE) df else df[0, ]

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(length(levels(df$label))))

  # main chart
  p <- ggplot(df, aes(x_val, y_val))

  # scatter plot for the main model
  p <- p + drwhy_geom_point(df, smooth, alpha_val = 0.65)

  # smoot curve for the main model
  if (smooth == TRUE)
    p <- p + drwhy_geom_smooth(maybe_smooth)

  # theme, colours, titles, axes, scales, etc.
  p <- p + theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3"),
          plot.subtitle = element_text(vjust = -1)) +
    scale_color_manual(values = rev(colours), breaks = levels(df$label), guide = guide_legend(nrow = 1))

  chart_title <- "Autocorrelation "
  x_lab <- as.character(df_temp$variable[1])

  if (x_lab == "Target variable") {
    p <- p + scale_x_continuous(breaks = scales::pretty_breaks())
    chart_subtitle <- paste0("of residuals ordered by predicted values")
  } else if (x_lab == "Observations") {
    p <- p + scale_x_continuous(breaks = 5, labels = "")
    chart_subtitle <- paste0("of unordered residuals")
  } else {
    p <- p + scale_x_continuous(breaks = scales::pretty_breaks())
    chart_subtitle <- paste0("of residuals ordered by ", x_lab)
}

p <- p + xlab("Residual i") + ylab("Residual i+1") + ggtitle(chart_title, subtitle = chart_subtitle)

return(p)
}


#' @rdname plot_autocorrelation
#' @export
plotAutocorrelation <- function(object, ..., smooth = FALSE) {
  message("Please note that 'plotAutocorrelation()' is now deprecated, it is better to use 'plot_autocorrelation()' instead.")
  plot_autocorrelation(object, ..., smooth = FALSE)
}
