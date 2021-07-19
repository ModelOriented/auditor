#' @title Peak Score
#'
#' @description This score is calculated on the basis of Peak test,
#'  which is used for checking for homoscedasticity of residuals in regression analyses.
#'
#' @param object An object of class \code{explainer} created with function
#'  \code{\link[DALEX]{explain}} from the DALEX package.
#' @param variable Name of model variable to order residuals.
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
#' score_peak(lm_audit)
#'
#' @importFrom stats update rstandard predict pf sd
#'
#'
#' @export

score_peak <- function(object, variable = NULL, data = NULL, y = NULL, ...) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  # inject new data to the explainer
  if (!is.null(data)){
    object$data <- data
    object$y <- y
    object$y_hat <- object$predict_function(object$model, data)
  }

  std_res <- object$residuals / sd(object$residuals)
  peaks <- sum( (abs(std_res) >= cummax(abs(std_res)))) / length(object$y)

    peak_results <- list(
      name = "peak",
      score = peaks)

  class(peak_results) <- "auditor_score"
  peak_results
}


#' @rdname score_peak
#' @export
scorePeak<- function(object) {
  warning("Please note that 'scorePeak()' is now deprecated, it is better to use 'score_peak()' instead.")
  score_peak(object)
}
