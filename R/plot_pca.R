#' @title Principal Component Analysis of models
#'
#' @description Principal Component Analysis of models residuals.
#' PCA can be used to assess the similarity of the models.
#'
#' @param object An object of class 'auditor_model_residual' created with \code{\link{model_residual}} function.
#' @param ... Other 'auditor_model_residual' objects to be plotted together.
#' @param scale A logical value indicating whether the models residuals should be scaled before the analysis.
#'
#' @return A ggplot object.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#'
#' # fit a model
#' lm_model <- lm(life_length ~ ., data = dragons)
#'
#' # use DALEX package to wrap up a model into explainer
#' lm_exp <- DALEX::explain(lm_model, data = dragons, y = dragons$life_length)
#'
#' # validate a model with auditor
#' library(auditor)
#' lm_mr <- model_residual(lm_exp)
#'
#' library(randomForest)
#' rf_model <- randomForest(life_length~., data = dragons)
#' rf_exp <- DALEX::explain(rf_model, data = dragons, y = dragons$life_length)
#' rf_mr <- model_residual(rf_exp)
#'
#' # plot results
#' plot_pca(lm_mr, rf_mr)
#'
#' @import ggplot2
#' @importFrom stats prcomp
#'
#' @export

plot_pca <- function(object, ..., scale = TRUE) {
  # some safeguard
  residuals <- label <- PC1 <- PC2 <- NULL

  # check if passed object is of class "auditor_model_residual"
  check_object(object, type = "res")

  # PCA object for ggplot object
  df <- make_dataframe(object, ..., type = "pca")
  pca_object <- prcomp(df, scale = scale)
  # colours for the model(s)
  colours <- rev(theme_drwhy_colors(length(names(df))))

  # arrows for models
  loadings <- pca_object$rotation
  std_dev <- pca_object$sdev

  arrows <- apply(loadings, 1, function(x, y){x*y}, std_dev)
  arrows <- data.frame(t(arrows))
  arrows$label <- rownames(arrows)

  arrows2 <- arrows
  arrows2$PC1 <- arrows2$PC2 <- 0
  arrows2 <- rbind(arrows, arrows2)

  # length of arrows


  # plot
  ggplot(data = data.frame(pca_object$x), aes(x = PC1, y = PC2)) +
    geom_point(colour = "grey", alpha = 0.75) +
    geom_hline(aes(yintercept = 0), size = 0.25) +
    geom_vline(aes(xintercept = 0), size = 0.25) +
    geom_line(data = arrows2, aes(PC1, PC2, colour = label)) +
    geom_segment(data = arrows, aes(x = 0, y = 0, xend = PC1, yend = PC2, colour = label),
                 arrow = grid::arrow(length = grid::unit(2, "points")), show.legend = FALSE) +
    ggtitle("Model PCA") +
    scale_color_manual(values = rev(colours), breaks = arrows$label, guide = guide_legend(nrow = 1)) +
    theme_drwhy()
}


#' @rdname plot_pca
#' @export
plotModelPCA <- function(object, ..., scale = TRUE) {
  message("Please note that 'plotModelPCA()' is now deprecated, it is better to use 'plot_pca()' instead.")
  plot_pca(object, ..., scale)
}
