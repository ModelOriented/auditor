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
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' score_dw(lm_au)
#'
#'
#' @return an object of class scoreAudit
#'
#' @export

score_dw <- function(object, variable = NULL){
  if(!("model_residual" %in% class(object) || "model_audit" %in% class(object))) stop("The function requires an object created with audit() or model_residuals().")
  if(!("model_residual" %in% class(object))) object <- model_residual(object, variable)

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
  class(result) <- "score_audit"
  return(result)
}


#' @rdname score_cooksdistance
#' @export
scoreDW <- function(object, variable = NULL) {
  message("Please note that 'scoreDW()' is now deprecated, it is better to use 'score_dw()' instead.")
  score_dw(object, variable)
}
