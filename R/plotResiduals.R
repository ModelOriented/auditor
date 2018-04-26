#' @title Plot Residuals vs Fitted or Variable Values
#'
#' @description A plot of residuals against fitted values or any variable values.
#'
#'
#' @param object An object of class modelAudit
#' @param variable name of modle variable for x-axis. If NULL fitted values are taken.
#' @param ... other modelAudit objects to be plotted together
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#'
#' @export
plotResiduals <- function(object, ..., variable=NULL){
  residuals <- values <- NULL
  if(is.null(variable)) variable <- "Fitted values"
  df <- generateResidualsDF(object, variable)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- rbind( df, generateResidualsDF(resp, variable) )
      }
    }
  }

  maybe_points <- if (length(unique(df$label)) ==1) df else df[0, ]


  ggplot(df, aes(values, residuals)) +
    geom_point(data = maybe_points) +
    geom_smooth(aes(color = label), method = "loess", se = FALSE) +
    xlab(variable) +
    ylab("Residuals") +
    ggtitle(paste0("Residuals vs ", variable)) +
    theme_light()
}


generateResidualsDF <- function(object, variable){
  if(variable == "Fitted values") {
    values <- object$fitted.values
  } else {
    values <- object$data[,variable]
  }
  n <- length(object$residuals)
  resultDF <- data.frame(values = values, residuals = object$residuals, label = object$label)
  resultDF <- dplyr::arrange(resultDF, values)
  return(resultDF)
}
