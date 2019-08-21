#' @title Residual Density Plot
#'
#' @description Density of model residuals.
#'
#' @param object An object of class 'auditor_model_residual' created with \code{\link{model_residual}} function.
#' @param ... Other 'auditor_model_residual' objects to be plotted together.
#' @param split Logical. Indicates whenever plot should be splitted by variable.
#' @param variable Variable name o split. Optional.
#'
#' @return ggplot object
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
#' plot_residual_density(mr_lm)
#' plot(mr_lm, type = "residual_density")
#'
#' library(randomForest)
#' model_rf <- randomForest(life_length~., data = dragons)
#' exp_rf <- DALEX::explain(model_rf, data = dragons, y = dragons$life_length)
#' mr_rf <- model_residual(exp_rf)
#' plot_residual_density(mr_lm, mr_rf)
#' plot(mr_lm, mr_rf, type = "residual_density")
#'
#' @seealso \code{\link{plot_residual}}
#'
#' @import ggplot2
#'
#' @rdname plot_residual_density
#'
#' @export
plot_residual_density <- function(object, ..., split = FALSE, variable = NULL) {
  if(!is.null(variable)) split <- TRUE
  if (split == FALSE && (!is.null(variable) && nchar(variable) > 1))
    stop("Please change argument `split` to `TRUE` if you want to plot residual density of a specific variable")

  # some safeguard
  `_residuals_` <- label <- div <- NULL
  # check if passed object is of class "auditor_model_residuals"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., variable = variable, type = "dens")


  # some helpfull objects
  df$`_ord_` <- paste(rev(as.numeric(df$`_label_`)), df$`_label_`)
  model_count <- length(levels(df$`_label_`))

  # arguments values differ depending on splitting or not
  if (split == TRUE) {
    var_split <- "`_div_`"
    colours <- theme_drwhy_colors(length(unique(df$`_div_`)))
    legend_pos <- "bottom"
    legend_just <- "center"
    split_by <- unique(df$`_div_`)
  } else if (split == FALSE) {
    var_split <- "`_label_`"
    colours <- theme_drwhy_colors(model_count)
    legend_pos <- "top"
    legend_just <- c(1, 0)
    if (model_count == 1) legend_pos <- "none"
    split_by <- unique(df$`_label_`)
  }

  p <- ggplot(data = df, aes(x = `_residuals_`)) +
    geom_density(alpha = 0.3, aes_string(fill = var_split)) +
    geom_rug(aes_string(color = var_split), alpha = 0.5) +
    geom_vline(xintercept = 0, colour = "darkgrey") +
    annotate("segment", x = -Inf, xend = Inf,  y = -Inf, yend = -Inf, colour = "#371ea3") +
    scale_color_manual(values = colours) +
    scale_fill_manual(values = colours, breaks = split_by) +
    guides(fill = guide_legend(override.aes = list(alpha = 0.5)), col = FALSE) +
    theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3"),
          legend.text = element_text(margin = margin(r = 5, l = 3)),
          legend.key = element_rect(colour = NA, fill = NA),
          legend.position = legend_pos,
          legend.justification = legend_just) +
    xlab("") + ylab("") + ggtitle("Residuals density")

  if (model_count > 1 && split == FALSE) {
    p
  } else {
    p <- p + facet_wrap(. ~ `_label_`, scales = "free_x", ncol = 2) +
      theme(strip.text = element_text(colour = "#160e3b", size = rel(1), face = "bold"),
            panel.spacing.y = unit(0.5, "lines"),
            strip.text.x = element_text(margin = margin(0.1, 0, 0.2, 0, "cm")))
    if (model_count == 1) p <- p + theme(strip.text = element_blank())
    p
  }
}


#' @rdname plot_residual_density
#' @export
plotResidualDensity <- function(object, ..., split = FALSE, variable = NULL) {
  message("Please note that 'plotResidualDensity()' is now deprecated, it is better to use 'plot_residual_density()' instead.")
  plot_residual_density(object, ..., split, variable)
}
