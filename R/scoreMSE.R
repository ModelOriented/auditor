#' @title Mean Square Error
#'
#' @description Mean Square Error.
#'
#' @param object An object of class ModelAudit.
#'
#' @return an object of class scoreAudit
#'
#' @seealso \code{\link{score}}
#'
#' @export


scoreMSE <- function(object){


  MSEResults <- list(
    name = "MSE",
    score = mean((object$y - object$fitted.values)^2)
    )

  class(MSEResults) <- "scoreAudit"
  return(MSEResults)
}



