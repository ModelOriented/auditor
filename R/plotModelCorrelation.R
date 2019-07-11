#' @title Model Correlation Plot
#'
#' @description Matrix of plots
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param ... Other modelAudit or modelResiduals objects to be plotted together.
#' @param values "fit" for model fitted values or "res" for residual values.
#'
#' @return Invisibly returns a \code{\link[gtable]{gtable}} object.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#'
#' library(randomForest)
#' rf_model <- randomForest(life_length~., data = dragons)
#' rf_au <- audit(rf_model, data = dragons, y = dragons$life_length)
#' plotModelCorrelation(lm_au, rf_au)
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import grid
#' @import gridExtra
#' @importFrom stats cor
#' @importFrom utils combn
#'
#' @export
plotModelCorrelation <- function(object, ..., values = "fit") {

  x <- y <- NULL

  # check if passed object is of class "modelResiduals" or "modelAudit"
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



#' Modify a modelCorrelation object by adding an ggplot2 object to all plots
#'
#' This operator allows you to add ggplot2 objects to a modelCorrelation object.
#'
#' @param e1 An object of class \code{ggplot} or \code{theme}
#' @param e2 A component to add to \code{e1}
#'
#' @export
#' @seealso \code{\link[ggplot2]{+.gg}} and \code{\link[ggplot2]{theme}}
#' @method + gg
#' @rdname gg-add
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#'
#' library(randomForest)
#' rf_model <- randomForest(life_length~., data = dragons)
#' rf_au <- audit(rf_model, data = dragons, y = dragons$life_length)
#'
#' plotModelCorrelation(lm_au, rf_au) + theme_bw()
#'
"+.gg" <- function(e1, e2) {

  if (! "modelCorrelationPlot" %in% class(e1)) {
    return(e1 %+% e2)
  }
  plot(e2)

}
