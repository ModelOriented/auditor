#' @title Durbin-Watson Score
#'
#' @description Autocorrelation based on Durbin-Watson Test.
#'
#' @param object object An object of class ModelAudit
#' @param variable name of dependent or independent variable to order residuals. If NULL the fitted values are taken.
#'
#' @importFrom car durbinWatsonTest
#'
#' @export

scoreDW <- function(object, variable = NULL){
  if(is.null(variable)) {
    variable <- "Fitted values"
    dataDW <- data.frame(variable=object$fitted.values, residuals = object$residuals)
  } else {
    dataDW <- data.frame(variable=object$data[,variable], residuals = object$residuals)
  }

    dataDWOrdered <- dplyr::arrange(dataDW, variable)
    residuals <- dataDWOrdered$residuals

  result <- list(
    name = "Durbin-Watson",
    score = durbinWatsonTest(residuals)
  )
  class(result) <- "scoreAudit"
  return(result)
}



