#' @title Principal Component Analysis of models
#'
#' @description Principal Component Analysis of models residuals.
#' PCA can be used to assess the similarity of the models.
#'
#' @param object An object of class ModelAudit
#' @param ... other modelAudit objects to be plotted together
#' @param scale a logical value indicating whether the models residuals should be scaled bfore the analysis.
#' @param invisible a text specifying the elements to be hidden on the plot. Default value is "none". Allowed values are "model", "observ".
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#' @importFrom factoextra fviz_pca_biplot
#' @importFrom stats prcomp
#'
#' @export


plotModelPCA <- function(object, ..., scale = TRUE, invisible = "none"){
  residuals <- label <- NULL
  df <- getModelPCADF(object)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- cbind( df, getModelPCADF(resp) )
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


getModelPCADF <- function(object){

  df <- data.frame(residuals = object$residuals)
  colnames(df)[1] <- object$label

  return(df)
}



