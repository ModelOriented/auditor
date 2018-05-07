#' @title Residual Density Plot
#'
#' @description Density of model residuals.
#'
#' @param object An object of class ModelAudit
#' @param ... other modelAudit objects to be plotted together
#' @param variable a variable name. Residuals will be plotted separately for different values of variable. or continuous variables, they will be separated by a mean.
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#'
#' @export


plotResidualDensity <- function(object, ..., variable = NULL){
  residuals <- label <- NULL
  df <- getResidDensDF(object, variable)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- rbind( df, getResidDensDF(resp, variable) )
      }
    }
  }


  if(!is.null(variable)) {
    p <- ggplot(df, aes(x = residuals, fill = variable)) +
      stat_density(alpha = 0.3, position="identity")+
      geom_vline(xintercept = 0) +
      geom_rug(aes(color = variable), alpha = 0.5) +
      facet_grid(label~.) +
      theme_light() +
      xlab("residuals") +
      ggtitle("Residual Density")

  } else {
    p <- ggplot(df, aes(x = residuals, fill = label)) +
      stat_density(alpha = 0.3, position="identity")+
      geom_vline(xintercept = 0) +
      geom_rug(aes(color = label), alpha = 0.5) +
      theme_light() +
      xlab("residuals") +
      ggtitle("Residual Density")
  }

  return(p)
}


getResidDensDF <- function(object, variable){

  df <- data.frame(residuals = object$residuals, label = object$label)
  if (!is.null(variable)) {
    modelData <- object$data

    if (class(modelData[, variable]) == "numeric") {
      varMean <- mean(modelData[,variable])
      df$variable <- ifelse(modelData[,variable] > varMean, paste(">", variable, "mean"), paste("<=", variable, "mean"))
    } else {
      df$variable <-modelData[,variable]
    }
  }

  return(df)
}

