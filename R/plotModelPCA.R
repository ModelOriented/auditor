#' @title Model PCA plot
#'
#' @description PCA for model residuals
#'
#' @param object An object of class ModelAudit
#' @param ... other modelAudit objects to be plotted together
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


plotModelPCA <- function(object, ...){
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

  res.pca <- prcomp(df, scale = TRUE)

  fviz_pca_biplot(res.pca,
                  repel = TRUE,
                  label = c("var"),
                  col.var = "#000000",
                  col.ind = "#d8d8d8",
                  arrowsize = 1
  )
}


getModelPCADF <- function(object){

  df <- data.frame(residuals = object$residuals)
  colnames(df)[1] <- object$label

  return(df)
}



