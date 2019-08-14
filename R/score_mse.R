#' @title Mean Square Error
#'
#' @description Mean Square Error.
#'
#' @param object An object of class 'explainer' created with function \code{\link[DALEX]{explain}} from the DALEX package.
#'
#' @return an object of class score_audit
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
#' score_mse(exp_lm)
#'
#' @seealso \code{\link{score}}
#'
#' @export


score_mse <- function(object){
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  mse_results <- list(
    name = "mae",
    score = mean((object$y - object$y_hat)^2)
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
