#' @title Area Over the Curve for RROC Curves
#'
#' @description The area over the Regression Receiver Operating Characteristic.
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
#' score_rroc(lm_audit)
#'
#'
#' @seealso \code{\link{plot_rroc}}
#'
#' @references Hernández-Orallo, José. 2013. "ROC Curves for Regression". Pattern Recognition 46 (12): 3395–3411.
#'
#' @rdname score_rroc
#'
#' @export


score_rroc <- function(object, data = NULL, y = NULL, ...) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  # inject new data to the explainer
  if (!is.null(data)){
    object$data <- data
    object$y <- y
    object$y_hat <- object$predict_function(object$model, data)
  }

  object <- model_residual(object)

  RROCF <- make_rroc_df(object)
  RROCF <- RROCF[RROCF$`_curve_` == TRUE,]
  x <- RROCF$`_rroc_x_`
  y <- RROCF$`_rroc_y_`

  aoc <- 0
  for (i in 2:(length(x) - 2)) {
    aoc <- aoc + 0.5 * (y[i+1] + y[i]) * (x[i+1] - x[i])
  }

  rroc_results <- list(
    name = "rroc",
    score = aoc
  )

  class(rroc_results) <- "auditor_score"
  rroc_results

}


#' @rdname score_rroc
#' @export
scoreRROC<- function(object) {
  warning("Please note that 'scoreRROC()' is now deprecated, it is better to use 'score_rroc()' instead.")
  score_rroc(object)
}
