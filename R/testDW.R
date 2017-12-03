#' @title Durbin-Watson Test
#'
#' @description Test for autocorrelation with the Durbin-Watson Test.
#'
#' @param model model
#' @param vars vector of variables - for some tests and plots observations in the model are ordered by the values of this variables. If NULL - all variables are taken.
#'
#' @importFrom car durbinWatsonTest
#'
#' @export

testDW <- function(model, vars = NULL){
  dataFromModel <- model.frame(model)
  dependentVariable <- dataFromModel[,1]
  if (is.null(vars))  vars <- colnames(dataFromModel)[-1]

  DWResults <- list()
  for(variable in vars){
    dataFromModel$residuals <- getResiduals(model, dataFromModel[, 1])
    dataFromModelOrdered <- arrange_(dataFromModel, variable)
    residuals <- dataFromModelOrdered$residuals
    DWStatistic <- durbinWatsonTest(residuals)
    DWResults[[variable]] <- DWStatistic
  }

  result <- list(
    name = "Durbin-Watson",
    statistics = DWResults
  )
  class(result) <- "testAudit"
  return(result)
}

