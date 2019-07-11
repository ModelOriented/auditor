#' @title Peak Score
#'
#' @description This score is calculated on the basis of Peak test, which is used for checking for homoscedasticity of residuals in regression analyses.
#'
#' @param object Object An object of class modelAudit or modelResidual.
#' @param variable Name of model variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' scorePeak(lm_au)
#'
#' @importFrom stats update rstandard predict pf sd
#'
#' @return an object of class scoreAudit
#'
#' @export

scorePeak <- function(object, variable = NULL){
  if(!("modelResiduals" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelResiduals().")
  if(!("modelResiduals" %in% class(object))) object <- modelResiduals(object, variable)

  peaks <- sum( (abs(object$std.res) >= cummax(abs(object$std.res)))) / nrow(object)

    PeakResults <- list(
      name = "Peak",
      score = peaks)

  class(PeakResults) <- "scoreAudit"
  return(PeakResults)
}
