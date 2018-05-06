#' @title Cook's Distances Plot
#'
#' @description Cook’s distance are used for estimate of the influence of an single observation.
#'
#'
#' @param object An object of class ModelAudit
#' @param nlabel number of observations with the biggest Cooks distances to be labeled
#' @param ... other arguments passed \code{\link{scoreCook}} to scoreCook.
#'
#' @details Cook’s distance is a tool for identifying observations that may negatively affect the model.
#' They may be also used for indicating regions of the design space where it would be good to obtain more observations.
#' Data points indicated by Cook’s distances are worth checking for validity.
#'
#' Cook’s Distances are calculated by removing the i-th observation from the data and recalculating the model.
#' It shows how much all the values in the model change when the i-th observation is removed.
#'
#' Models of classes other than lm and glm the distances are computed directly from the definition,
#' so this may take a while. In this example we will compute them for a linear model.
#'
#' @import ggplot2
#' @importFrom dplyr desc
#'
#' @export
plotCook <- function(object, nlabel = 3, ...){
  index <- cooks.dist <- big <- nameIndex <- NULL

  plotData <- data.frame(cooks.dist = scoreCook(object, ...), index = 1:nrow(object$data),
                         nameIndex = rownames(object$data))
  plotData <- dplyr::arrange(plotData, desc(cooks.dist))
  plotData$big <- c(rep(TRUE, nlabel), rep(FALSE, nrow(object$data)-nlabel))

  ggplot(plotData, aes(index, cooks.dist)) +
      geom_point() +
      geom_text(data = subset(plotData, big==TRUE), aes(label=as.character(nameIndex)),hjust=-0.2,vjust=-0.2) +
      xlab("observation index") +
      ylab("cooks distance") +
      ggtitle("Influence of observations") +
      theme_light()
}

