#' @title Principal Component Analysis of models
#'
#' @description Principal Component Analysis of models residuals.
#' PCA can be used to assess the similarity of the models.
#'
#' @param object An object of class modelAudit or modelResiduals,
#' @param ... Other modelAudit or modelResiduals objects to be plotted together.
#' @param scale A logical value indicating whether the models residuals should be scaled before the analysis.
#'
#' @return ggplot object
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' library(randomForest)
#' rf_model <- randomForest(prestige~education + women + income, data = Prestige)
#' rf_au <- audit(rf_model, data = Prestige, y = Prestige$prestige)
#' plotModelPCA(lm_au, rf_au)
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#' @importFrom factoextra fviz_pca_biplot
#' @importFrom stats prcomp
#'
#' @export


plotModelPCA <- function(object, ..., scale = TRUE) {

  # some safeguard
  residuals <- label <- PC1 <- PC2 <- NULL

  # check if passed object is of class "modelResiduals" or "modelAudit"
  check_object(object, type = "res")

  # PCA object for ggplot object
  df <- make_dataframe(object, ..., type = "pca")
  pca_object <- prcomp(df, scale = scale)

  # colours for the model(s)
  colours <- rev(theme_drwhy_colors(length(names(df))))

  # arrows for models
  arrows <- data.frame(pca_object$rotation)
  arrows$label <- rownames(arrows)
  arrows2 <- arrows
  arrows2$PC1 <- arrows2$PC2 <- 0
  arrows2 <- rbind(arrows, arrows2)

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
