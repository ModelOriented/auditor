#' @title Influence of observations plot
#'
#' @description Cook’s distances are used for estimate the influence of an single observation.
#'
#'
#' @param object An object of class ModelAudit.
#' @param nlabel Number of observations with the biggest Cook's distances to be labeled.
#' @param ... Other arguments passed to \code{\link{scoreCooksDistance}}.
#'
#' @details Cook’s distance is a tool for identifying observations that may negatively affect the model.
#' They may be also used for indicating regions of the design space where it would be good to obtain more observations.
#' Data points indicated by Cook’s distances are worth checking for validity.
#'
#' Cook’s Distances are calculated by removing the i-th observation from the data and recalculating the model.
#' It shows how much all the values in the model change when the i-th observation is removed.
#'
#' For model classes other than lm and glm the distances are computed directly from the definition.
#'
#' @import ggplot2
#'
#' @export
plotCooksDistance <- function(object, nlabel = 3, ...){
  index <- cooks.dist <- big <- nameIndex <- NULL

  plotData <- data.frame(cooks.dist = scoreCooksDistance(object, ...), index = 1:nrow(object$data),
                         nameIndex = rownames(object$data))
  plotData <- plotData[order(-plotData$cooks.dist),]
  plotData$big <- c(rep(TRUE, nlabel), rep(FALSE, nrow(object$data)-nlabel))

  ggplot(plotData, aes(index, cooks.dist)) +
      geom_point() +
      geom_text(data = subset(plotData, big==TRUE), aes(label=as.character(nameIndex)),hjust=-0.2,vjust=-0.2) +
      xlab("observation index") +
      ylab("cook's distance") +
      ggtitle("Influence of observations") +
      theme_light()
}
