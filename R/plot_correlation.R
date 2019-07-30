#' @title Correlation of Model's Residuals Plot
#'
#' @description Matrix of plots. Left-down triangle consists of plots of fitted values (aternatively residuals),
#' on the diagonal there are density plots of fitted values (alternatively residuals), in the right-top triangle
#' there are correlations between fitte dvalues (alternatively residuals).
#'
#' @param object An object of class 'auditor_model_residual' created with \code{\link{model_residual}} function.
#' @param ... Other 'auditor_model_residual' objects to be plotted together.
#' @param values "fit" for model fitted values or "res" for residual values.
#'
#' @return Invisibly returns a \code{\link[gtable]{gtable}} object.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_exp <- DALEX::explain(lm_model, data = dragons, y = dragons$life_length)
#' library(auditor)
#' lm_mr <- model_residual(lm_exp)
#'
#' library(randomForest)
#' rf_model <- randomForest(life_length~., data = dragons)
#' rf_exp <- DALEX::explain(rf_model, data = dragons, y = dragons$life_length)
#' rf_mr <- model_residual(rf_exp)
#' plot_correlation(lm_mr, rf_mr)
#'
#'
#' @import grid
#' @import gridExtra
#' @importFrom stats cor
#' @importFrom utils combn
#'
#' @export
plot_correlation <- function(object, ..., values = "fit") {

  x <- y <- NULL

  # check if passed object is of class "auditor_model_residual"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., values = values, type = "corr")

  # plots of density
  vars <- names(df)
  lab_x <- vars %in% vars[length(vars)]
  lab_y <- vars %in% vars[1]
  lim_y <- max(sapply(vars, function(x) max(density(df[, x])[["y"]])))

  args <- mapply(c, vars, lab_x, lab_y, lim_y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  plots_dens <- lapply(args, corr_density, df)

  # plots of fitted values / residuals
  lay_matrix <- prepare_matrix(df)
  slots <- lay_matrix[lower.tri(lay_matrix)]
  lab_x <- slots %in% lay_matrix[nrow(lay_matrix), ]
  lab_y <- slots %in% lay_matrix[, 1]
  vars <- combn(names(df), 2, simplify = FALSE)

  args <- mapply(c, vars, lab_x, lab_y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  plots_scat <- lapply(args, corr_points, df)

  # correlation coefficients
  coefs <- as.vector(cor(df))
  coefs <- round(unique(coefs[coefs != 1]), 3)
  coefs <- paste0("Correlation: \n", coefs)
  coefs <- lapply(coefs, function(x) {
    textGrob(label = x, gp = gpar(col = "#160e3b", fontsize = 10))
  })

  # pairs of plot
  a <- arrangeGrob(grobs = c(plots_dens, plots_scat, coefs), layout_matrix = lay_matrix)
  grid.newpage()
  grid.draw(a)
  class(a) <- c("modelCorrelationPlot", class(a))
  invisible(a)
}

#' @rdname plot_correlation
#' @export
plotModelCorrelation <- function(object, ..., values = "fit") {
  message("Please note that 'plotModelCorrelation()' is now deprecated, it is better to use 'plot_correlation()' instead.")
  plot_correlation(object, ..., values)
}
