#' @title Durbin-Watson Score
#'
#' @description Score based on Durbin-Watson test statistic.
#' The score value is helpful in comparing models. It is worth pointing out that results of tests like p-value makes sense only
#' when the test assumptions are satisfied. Otherwise test statistic may be considered as a score.
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param variable Name of model variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' scoreDW(lm_au)
#'
#'
#' @return an object of class scoreAudit
#'
#' @export

scoreDW <- function(object, variable = NULL){
  if(!("modelResiduals" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelResiduals().")
  if(!("modelResiduals" %in% class(object))) object <- modelResiduals(object, variable)

  residuals <- object$res
  max_lag <- 1
  n <-  length(residuals)
  durbin_watson <- rep(0, max_lag)
  den <- sum(residuals^2)
  for (lag in 1:max_lag){
    durbin_watson[lag] <- (sum((residuals[(lag+1):n] - residuals[1:(n-lag)])^2))/den
  }

  durbin_watson

  result <- list(
    name = "Durbin-Watson",
    score = durbin_watson
  )
  class(result) <- "scoreAudit"
  return(result)
}



