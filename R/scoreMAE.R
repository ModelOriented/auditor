#' @title Mean Absolute Error
#'
#' @description Mean Absolute Error.
#'
#' @param object An object of class ModelAudit or modelResiduals.
#'
#' @return an object of class scoreAudit
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' scoreMAE(lm_au)
#'
#'
#' @seealso \code{\link{score}}
#'
#' @export


scoreMAE <- function(object){
  if(!("modelResiduals" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelResiduals().")

  MAEResults <- list(
    name = "MAE",
    score = mean(abs(object$y - object$fitted.values))
    )

  class(MAEResults) <- "scoreAudit"
  return(MAEResults)
}



