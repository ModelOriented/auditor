#' @title Durbin-Watson Score
#'
#' @description Score based on Durbin-Watson test statistic.
#' The score value is helpful in comparing models. It is worth pointing out that results of tests like p-value makes sense only
#' when the test assumptions are satisfied. Otherwise test statistic may be considered as a score.
#'
#' @param object An object of class 'explainer' created with function \code{\link[DALEX]{explain}} from the DALEX package.
#' @param variable Name of model variable to order residuals.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_exp <- DALEX::explain(lm_model, data = dragons, y = dragons$life_length)
#' score_dw(lm_exp)
#'
#'
#' @return an object of class 'scoreAudit'auditor_score'
#'
#' @rdname score_dw
#'
#' @export

score_dw <- function(object, variable = NULL){
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  object <- model_residual(object)
  if(!is.null(variable)) object <- object[order(object[ ,variable]), ]
  residuals <- object$`_residuals_`

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
  class(result) <- "auditor_score"
  return(result)
}


#' @rdname score_dw
#' @export
scoreDW <- function(object, variable = NULL) {
  message("Please note that 'scoreDW()' is now deprecated, it is better to use 'score_dw()' instead.")
  score_dw(object, variable)
}
