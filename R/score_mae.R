#' @title Mean Absolute Error
#'
#' @description Mean Absolute Error.
#'
#' @param object An object of class 'Model_audit' or 'model_residual'.
#'
#' @return an object of class 'score_audit'
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' score_mae(lm_au)
#'
#'
#' @seealso \code{\link{score}}
#'
#' @export


score_mae <- function(object){
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  MAEResults <- list(
    name = "mae",
    score = mean(abs(object$y - object$y_hat))
    )

  class(MAEResults) <- "score_audit"
  return(MAEResults)
}



#' @rdname score_mae
#' @export
scoreMAE<- function(object) {
  message("Please note that 'scoreMAE()' is now deprecated, it is better to use 'score_mae()' instead.")
  score_dw(object)
}
