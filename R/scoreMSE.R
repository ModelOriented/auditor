#' @title Mean Square Error
#'
#' @description Mean Square Error.
#'
#' @param object An object of class modelAudit or modelResiduals.
#'
#' @return an object of class scoreAudit
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
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



