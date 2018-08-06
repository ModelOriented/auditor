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
#'
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' plotCooksDistance(lm_au)
#'
#' @import ggplot2
#'
#' @export
plotCooksDistance <- function(object, nlabel = 3, ...){
  if(!("observationInfluence" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or observationInfluence().")
  if(!("observationInfluence" %in% class(object))) object <- observationInfluence(object)
  index <- cooks.dist <- big <- nameIndex <- NULL

  df <- object
  df$big <- c(rep(TRUE, nlabel), rep(FALSE, nrow(df)-nlabel))

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if("modelAudit" %in% class(resp)) resp <- observationInfluence(resp)
      if("observationInfluence" %in% class(resp)) {
        resp$big <- c(rep(TRUE, nlabel), rep(FALSE, nrow(resp)-nlabel))
        df <- rbind(df, resp)
      }
    }
  }

  ggplot(df, aes(index, cooks.dist)) +
      geom_point() +
      geom_text(data = subset(df, big==TRUE), aes(label=as.character(index)), color = "grey") +
      facet_grid(label ~ .) +
      xlab("observation index") +
      ylab("cook's distance") +
      ggtitle("Influence of observations") +
      theme_light()
}
