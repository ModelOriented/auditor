#' @title Principal Component Analysis of models
#'
#' @description Principal Component Analysis of models residuals.
#' PCA can be used to assess the similarity of the models.
#'
#' @param object An object of class modelAudit or modelResiduals,
#' @param ... Other modelAudit or modelResiduals objects to be plotted together.
#' @param scale A logical value indicating whether the models residuals should be scaled before the analysis.
#' @param invisible A text specifying the elements to be hidden on the plot. Default value is "none". Allowed values are "model", "observ".
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


plotModelPCA <- function(object, ..., scale = TRUE, invisible = "none"){
  if(!("modelResiduals" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelResiduals().")
  if(!("modelResiduals" %in% class(object))) object <- modelResiduals(object)
  residuals <- label <- NULL

    df <- data.frame(y = object$res)
    colnames(df)[1] <- as.character(object$label[1])


  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if("modelAudit" %in% class(resp)) resp <-  modelResiduals(resp)
      if("modelResiduals" %in% class(resp)){
        df_tmp <- data.frame(resp$res)
        colnames(df_tmp)[1] <- as.character(resp$label[1])
        df <- cbind(df, df_tmp)
      }
    }
  }

  res_pca <- prcomp(df, scale = scale)

  if(invisible == "model") invisible <- "var"
  if(invisible == "observ") invisible <- "observ"

  fviz_pca_biplot(res_pca,
                  repel = TRUE,
                  label = c("var"),
                  col.var = "#000000",
                  col.ind = "#d8d8d8",
                  invisible = invisible,
                  title = "Model PCA") +
    theme_light()
}
