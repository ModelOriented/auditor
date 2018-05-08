#' @title Root Mean Square Error
#'
#' @description Root Mean Square Error.
#'
#' @param object An object of class ModelAudit.
#'
#' @return an object of class scoreAudit
#'
#' @seealso \code{\link{score}}
#'
#' @export


scoreRMSE <- function(object){


  MSEResults <- list(
    name = "RMSE",
    score = sqrt(mean((object$y - object$fitted.values)^2))
    )

  class(MSEResults) <- "scoreAudit"
  return(MSEResults)
}



