#' @title Root Mean Square Error
#'
#' @description Root Mean Square Error.
#'
#' @param object An object of class ModelAudit.
#'
#' @return an object of class scoreAudit
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_exp <- DALEX::explain(lm_model, data = dragons, y = dragons$life_length)
#' score_rmse(lm_exp)
#'
#'
#' @seealso \code{\link{score}}
#'
#' @export


score_rmse <- function(object){
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  mse_results <- list(
    name = "rmse",
    score = sqrt(mean((object$y - object$y_hat)^2))
    )

  class(mse_results) <- "auditor_score"
  mse_results
}

#' @rdname score_rmse
#' @export
scoreRMSE<- function(object) {
  message("Please note that 'scoreRMSE()' is now deprecated, it is better to use 'score_rmse()' instead.")
  score_rmse(object)
}


