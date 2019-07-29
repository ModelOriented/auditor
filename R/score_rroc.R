#' @title Area Over the Curve for RROC Curves
#'
#' @description The area over the Regression Receiver Operating Characteristic.
#'
#' @param object An object of class 'model_audit'.
#'
#' @return an object of class 'score_audit'.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' score_rroc(lm_au)
#'
#'
#' @seealso \code{\link{plot_rroc}}
#'
#' @references Hernández-Orallo, José. 2013. ‘ROC Curves for Regression’. Pattern Recognition 46 (12): 3395–3411.
#'
#' @export


score_rroc <- function(object) {

  check_object(object, type = "res")

  if (!"model_residual" %in% class(object)) object <- model_residual(object, variable = NULL)

  RROCF <- make_rroc_df(object)
  RROCF <- RROCF[RROCF$curve == TRUE,]
  x <- RROCF$rroc_x
  y <- RROCF$rroc_y

  aoc <- 0
  for (i in 2:(length(x) - 2)) {
    aoc <- aoc + 0.5 * (y[i+1] + y[i]) * (x[i+1] - x[i])
  }

  rroc_results <- list(
    name = "rroc",
    score = aoc
  )

  class(rroc_results) <- "score_audit"
  rroc_results

}


#' @rdname score_roc
#' @export
scoreRROC<- function(object) {
  message("Please note that 'scoreRROC()' is now deprecated, it is better to use 'score_rroc()' instead.")
  score_rroc(object)
}
