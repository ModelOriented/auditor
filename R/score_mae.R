#' @title Mean Absolute Error
#'
#' @description Mean Absolute Error.
#'
#' @param object An object of class 'explainer' created with function \code{\link[DALEX]{explain}} from the DALEX package.
#'
#' @return an object of class 'score_audit'
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#'
#' # fit a model
#' model_lm <- lm(life_length ~ ., data = dragons)
#'
#' # create an explainer
#' exp_lm <- DALEX::explain(model_lm, data = dragons, y = dragons$life_length)
#'
#' # calculate score
#' score_mae(exp_lm)
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

  class(MAEResults) <- "auditor_score"
  return(MAEResults)
}



#' @rdname score_mae
#' @export
scoreMAE<- function(object) {
  message("Please note that 'scoreMAE()' is now deprecated, it is better to use 'score_mae()' instead.")
  score_dw(object)
}
