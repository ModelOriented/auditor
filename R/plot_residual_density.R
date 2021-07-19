#' @title Residual Density Plot
#'
#' @description Density of model residuals.
#'
#' @param object An object of class \code{auditor_model_residual} created with \code{\link{model_residual}} function.
#' @param variable Split plot by variable's factor level or median.
#' If \code{variable="_y_"}, the plot will be split by actual response (\code{y} parameter
#' passed to the \code{\link[DALEX]{explain}} function).
#' If \code{variable = "_y_hat_"} the plot will be split by predicted response.
#' If \code{variable = NULL}, the plot will be split by observation index
#' If \code{variable = ""} plot is not split (default option).
#' @param ... Other \code{auditor_model_residual} objects to be plotted together.
#' @param show_rugs Adds rugs layer to the plot. By default it's TRUE
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
#'
#' # plot results
#' plot_residual_density(mr_lm)
#' plot(mr_lm, type = "residual_density")
#'
#' library(randomForest)
#' model_rf <- randomForest(life_length~., data = dragons)
#' rf_audit <- audit(model_rf, data = dragons, y = dragons$life_length)
#' mr_rf <- model_residual(rf_audit)
#' plot_residual_density(mr_lm, mr_rf)
#' plot(mr_lm, mr_rf, type = "residual_density")
#'
#' @seealso \code{\link{plot_residual}}
#'
#'
#' @rdname plot_residual_density
#'
#' @export
plot_residual_density <- function(object, ..., variable = "", show_rugs = TRUE) {
  # some safeguard
  `_residuals_` <- `_label_` <- label <- div <- NULL

  # check if passed object is of class "auditor_model_residuals"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., variable = variable, type = "dens")

  # some helpfull objects
  model_count <- nlevels(df$`_label_`)
  colours <- theme_drwhy_colors(model_count)
  df$`_ord_` <- paste(rev(as.numeric(df$`_label_`)), df$`_label_`)

  # faceting plot
  split <- TRUE

  if (!is.null(variable)) {
    if (variable == "") split <- FALSE
  }

  # arguments differ depending on splitting or not
  legend_pos <- "top"
  legend_just <- c(1, 0)

  if (split == TRUE) {
    legend_pos <- "bottom"
    legend_just <- "center"
  }

  if (model_count == 1) legend_pos <- "none"

  # set value for title
  if (is.null(variable)) {
    lab <- "observations"
    split <- TRUE
  } else if (variable == "_y_")  {
    lab <- "target variable"
  } else if (variable == "_y_hat_") {
    lab <- "actual response"
  } else {
    lab <- as.character(variable)
  }

  p <- ggplot(data = df, aes(x = `_residuals_`)) +
    geom_density(alpha = 0.3, aes(fill = `_label_`)) +
    geom_vline(xintercept = 0, colour = "darkgrey") +
    annotate("segment", x = -Inf, xend = Inf,  y = -Inf, yend = -Inf, colour = "#371ea3") +
    scale_color_manual(values = colours) +
    scale_fill_manual(values = colours, breaks = unique(df$`_label_`)) +
    guides(fill = guide_legend(override.aes = list(alpha = 0.5))) +
    theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3"),
          legend.text = element_text(margin = margin(r = 5, l = 3)),
          legend.key = element_rect(colour = NA, fill = NA),
          legend.position = legend_pos,
          legend.justification = legend_just) +
    xlab("") + ylab("") + ggtitle(paste0("Residuals density by ", lab))

  if (show_rugs) {
    p <- p + geom_rug(aes(color = `_label_`), alpha = 0.5, show.legend = FALSE)
  }

  if (split == FALSE) {
    p
  } else {
    p + facet_wrap(. ~ `_div_`, scales = "free_x", ncol = 2) +
      theme(strip.text = element_text(colour = "#160e3b", size = rel(0.9), hjust = 0.5),
            panel.spacing.x = unit(1.2, "lines"),
            strip.text.x = element_text(margin = margin(0.5, 0, 0.2, 0, "cm")))
  }
}


#' @rdname plot_residual_density
#' @export
plotResidualDensity <- function(object, ..., variable = NULL) {
  warning("Please note that 'plotResidualDensity()' is now deprecated, it is better to use 'plot_residual_density()' instead.")
  plot_residual_density(object, ..., variable = variable)
}
