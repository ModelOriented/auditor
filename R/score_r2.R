#' @title R-squared
#'
#' @description The R2 is the coefficient of determination,
#' An R2 coefficient equals 0 means that model explains none of the variability of the response.
#' An R2 coefficient equals 1 means that model explains all the variability of the response.
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
#' # use DALEX package to wrap up a model into explainer
#' lm_audit <- audit(model_lm, data = dragons, y = dragons$life_length)
#'
#' # calculate score with auditor
#' score_r2(lm_audit)
#'
#' @seealso \code{\link{score}}
#'
#' @export


score_r2 <- function(object, data = NULL, y = NULL, ...) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  # inject new data to the explainer
  if (!is.null(data)) {
    object$data <- data
    model <- object$model
    y_hat <- object$predict_function(model, data)
  } else {
    y_hat <- object$y_hat
  }

  y <- object$y

  # residual sum of squares
  rss <- sum((y_hat - y) ^ 2)
  # total sum of squares
  tss <- sum((y - mean(y)) ^ 2)
  r2 <- 1 - rss/tss

  results <- list(
    name = "r2",
    score = r2
  )

  class(results) <- "auditor_score"
  return(results)
}

