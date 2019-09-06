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
#' library(randomForest)
#' model_rf <- randomForest(life_length~., data = dragons)
#' exp_rf <- DALEX::explain(model_rf, data = dragons, y = dragons$life_length)
#' mr_rf <- model_residual(exp_rf)
#'
#' # plot results
#' plot_correlation(mr_lm, mr_rf)
#' plot(mr_lm, mr_rf, type = "correlation")
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
  colnames(df)[colnames(df)=="_y_"] <- "y"
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
  vars <- names(df)
  vars <- combn(vars, 2, simplify = FALSE)

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
  class(a) <- c("model_correlation_plot", class(a))
  invisible(a)
}

#' @rdname plot_correlation
#' @noRd
#' @export
plotModelCorrelation <- function(object, ..., values = "fit") {
  message("Please note that 'plotModelCorrelation()' is now deprecated, it is better to use 'plot_correlation()' instead.")
  plot_correlation(object, ..., values)
}
