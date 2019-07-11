#' @title Root Mean Square Error
#'
#' @description Root Mean Square Error.
#'
#' @param object An object of class ModelAudit.
#'
#' @return an object of class scoreAudit
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' scoreRMSE(lm_au)
#'
#'
#' @seealso \code{\link{score}}
#'
#' @export


scoreRMSE <- function(object){
  if(!("modelResiduals" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelResiduals().")

  MSEResults <- list(
    name = "RMSE",
    score = sqrt(mean((object$y - object$fitted.values)^2))
    )

  class(MSEResults) <- "scoreAudit"
  return(MSEResults)
}



