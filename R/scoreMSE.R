#' @title Mean Square Error
#'
#' @description Mean Square Error.
#'
#' @param object An object of class modelAudit or modelResiduals.
#'
#' @return an object of class scoreAudit
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' scoreMSE(lm_au)
#'
#' @seealso \code{\link{score}}
#'
#' @export


scoreMSE <- function(object){
  if(!("modelResiduals" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelResiduals().")

  MSEResults <- list(
    name = "MSE",
    score = mean((object$y - object$fitted.values)^2)
    )

  class(MSEResults) <- "scoreAudit"
  return(MSEResults)
}



