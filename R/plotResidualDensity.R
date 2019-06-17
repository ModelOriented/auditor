#' @title Residual Density Plot
#'
#' @description Density of model residuals.
#'
#' @param object An object of class ModelAudit.
#' @param ... Other modelAudit objects to be plotted together.
#' @param split Logical. Indicates whenever plot should be splitted by variable.
#' @param variable variable name o split. Optional. Should be provided  only for modelAudit object.
#'
#' @return ggplot object
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' plotResidualDensity(lm_au)
#'
#' library(randomForest)
#' rf_model <- randomForest(prestige~education + women + income, data = Prestige)
#' rf_au <- audit(rf_model, data = Prestige, y = Prestige$prestige)
#' plotResidualDensity(lm_au, rf_au)
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#'
#' @export
plotResidualDensity <- function(object, ..., variable = NULL, split = FALSE) {
  # some safeguard
  res <- label <- div <- NULL

  # check if passed object is of class "modelResiduals" or "modelAudit"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., variable = variable, type = "dens")

  # some helpfull objects
  df$ord <- paste(rev(as.numeric(df$label)), df$label)
  model_count <- length(levels(df$label))

  # arguments values differ depending on splitting or not
  if (split == TRUE) {
    var_split <- "div"
    colours <- theme_drwhy_colors(length(unique(df$div)))
    legend_pos <- "bottom"
    legend_just <- "center"
    split_by <- unique(df$div)
  } else if (split == FALSE) {
    var_split <- "label"
    colours <- theme_drwhy_colors(model_count)
    legend_pos <- "top"
    legend_just <- c(1, 0)
    if (model_count == 1) legend_pos <- "none"
    split_by <- unique(df$label)
  }

  p <- ggplot(data = df, aes(x = res)) +
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
    p <- p + facet_wrap(. ~ label, scales = "free_x", ncol = 2) +
      theme(strip.text = element_text(colour = "#160e3b", size = rel(1), face = "bold"),
            panel.margin.y = unit(0.5, "lines"),
            strip.text.x = element_text(margin = margin(0.1, 0, 0.2, 0, "cm")))
    if (model_count == 1) p <- p + theme(strip.text = element_blank())
    p
  }
}
