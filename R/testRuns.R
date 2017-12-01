#' @title Runs Test
#'
#' @description Note that this test is not very strong. It utilizes only signs of the residuals.
#'
#' @param model model
#' @param vars name of variable - the observations in the model are ordered by the values of this variable. If NULL - first variable is taken.
#'
#' @importFrom tseries runs.test
#' @importFrom broom augment
#' @importFrom dplyr arrange
#'
#' @export

testRuns <- function(model, vars = NULL){
  dataFromModel <- model.frame(model)
  dependentVariable <- dataFromModel[,1]
  if (is.null(vars))  vars <- colnames(dataFromModel)[-1]

  RunsStatistics <- list()
  for(variable in vars){
    dataFromModelOrdered <- arrange_(dataFromModel, variable)
    residuals <- getResiduals(model, dataFromModelOrdered[, 1])
    signumOfResiduals <- factor(sign(residuals))
    RunsTested <- runs.test(signumOfResiduals)
    RunsStatistic <- RunsTested$statistic[[1]]
    pValue = RunsTested$p.value
    RunsStatistics[[variable]] <- list(statistic = RunsStatistic, pValue = pValue)
  }


  result <- list(
    name = "Runs test",
    statistics = RunsStatistics
  )
  class(result) <- "testAudit"
  return(result)
}

