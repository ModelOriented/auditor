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
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' score_rmse(lm_au)
#'
#'
#' @seealso \code{\link{score}}
#'
#' @export


score_rmse <- function(object){
  if(!("model_residual" %in% class(object) || "model_audit" %in% class(object))) stop("The function requires an object created with 'audit()' or 'model_residuals()'.")

  mse_results <- list(
    name = "rmse",
    score = sqrt(mean((object$y - object$fitted_values)^2))
    )

  class(mse_results) <- "score_audit"
  mse_results
}

#' @rdname score_rmse
#' @export
scoreRMSE<- function(object) {
  message("Please note that 'scoreRMSE()' is now deprecated, it is better to use 'score_rmse()' instead.")
  score_rmse(object)
}


