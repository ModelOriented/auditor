#' @title Root Mean Square Error
#'
#' @description Root Mean Square Error.
#'
#' @param object An object of class \code{explainer} created with function
#'  \code{\link[DALEX]{explain}} from the DALEX package.
#' @param data New data that will be used to calculate the score.
#'  Pass \code{NULL} if you want to use \code{data} from \code{object}.
#' @param y New y parameter will be used to calculate score.
#' @param ... Other arguments dependent on the type of score.
#'
#' @return An object of class \code{auditor_score}.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#'
#' # fit a model
#' model_lm <- lm(life_length ~ ., data = dragons)
#'
#' # create an explainer
#' lm_audit <- audit(model_lm, data = dragons, y = dragons$life_length)
#'
#' # calculate score
#' score_rmse(lm_audit)
#'
#'
#' @seealso \code{\link{score}}
#'
#' @export


score_rmse <- function(object, data = NULL, y = NULL, ...) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  # inject new data to the explainer
  if (!is.null(data)){
    object$data <- data
    object$y <- y
    object$y_hat <- object$predict_function(object$model, data)
  }

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
  warning("Please note that 'scoreRMSE()' is now deprecated, it is better to use 'score_rmse()' instead.")
  score_rmse(object)
}


