#' @title Mean Absolute Error
#'
#' @description Mean Absolute Error.
#'
#' @param object An object of class ModelAudit or modelResiduals.
#'
#' @return an object of class scoreAudit
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
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



