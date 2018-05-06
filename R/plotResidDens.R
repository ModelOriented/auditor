#' @title Residual Density Plot
#'
#' @description Density of model residuals.
#'
#' @param object An object of class ModelAudit
#' @param ... other modelAudit objects to be plotted together
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#'
#' @export


plotResidDens <- function(object, ...){
  residuals <- label <- NULL
  df <- getResidDensDF(object)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- rbind( df, getResidDensDF(resp) )
      }
    }
  }


  p <- ggplot(df, aes(x = residuals, fill = label)) +
    stat_density(color = "black", alpha = 0.3,  position = "identity")+
    geom_vline(xintercept = 0) +
    geom_rug(aes(color = label), alpha = 0.5) +
    theme_light() +
    xlab("residuals") +
    ggtitle("Residual Density")

  return(p)
}


getResidDensDF <- function(object){

  df <- data.frame(residuals = object$residuals, label = object$label)

  return(df)
}



