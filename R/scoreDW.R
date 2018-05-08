#' @title Durbin-Watson Score
#'
#' @description Score based on Durbin-Watson test statistic.
#' The score value is helpful in comparing models. It is worth ponting out that results of tests like p-value makes sense only
#' when the test assumptions are satisfied. Otherwise test statistic may be considered as a score.
#'
#' @param object object An object of class ModelAudit
#' @param variable Name of model variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#'
#' @importFrom car durbinWatsonTest
#'
#' @return an object of class scoreAudit
#'
#' @export

scoreDW <- function(object, variable = NULL){

  orderedResiduals <- orderResidualsDF(object, variable)

  result <- list(
    name = "Durbin-Watson",
    score = durbinWatsonTest(orderedResiduals)
  )
  class(result) <- "scoreAudit"
  return(result)
}



