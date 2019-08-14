#' @title Peak Score
#'
#' @description This score is calculated on the basis of Peak test, which is used for checking for homoscedasticity of residuals in regression analyses.
#'
#' @param object An object of class 'explainer' created with function \code{\link[DALEX]{explain}} from the DALEX package.
#' @param variable Name of model variable to order residuals.
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
#' score_peak(exp_lm)
#'
#' @importFrom stats update rstandard predict pf sd
#'
#' @return an object of class 'auditor_score'
#'
#' @export

score_peak <- function(object, variable = NULL){
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

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
  message("Please note that 'scorePeak()' is now deprecated, it is better to use 'score_peak()' instead.")
  score_peak(object)
}
