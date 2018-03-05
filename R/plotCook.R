#' @title Cook plot
#'
#' @description Cooks distances
#'
#'
#' @param object An object of class ModelAudit
#' @param nlabel number of observations with the biggest Cooks distances to be labeled
#' @param ... other arguments passed \code{\link{scoreCook}} to scoreCook.
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

