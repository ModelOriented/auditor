#' @title Mean Square Error
#'
#' @description Mean Square Error.
#'
#' @param object An object of class 'model_audit' or 'model_residual'.
#'
#' @return an object of class score_audit
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' score_mse(lm_au)
#'
#' @seealso \code{\link{score}}
#'
#' @export


score_mse <- function(object){
  if(!("model_residual" %in% class(object) || "model_audit" %in% class(object))) stop("The function requires an object created with 'audit()' or 'model_residual()'.")

  mse_results <- list(
    name = "mae",
    score = mean((object$y - object$fitted.values)^2)
    )

  class(mse_results) <- "score_audit"
  mse_results
}



#' @rdname score_mse
#' @export
scoreMSE<- function(object) {
  message("Please note that 'scoreMSE()' is now deprecated, it is better to use 'score_mse()' instead.")
  score_mse(object)
}
