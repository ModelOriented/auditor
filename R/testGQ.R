#' @title Goldfeld-Quandt Test
#'
#' @description Test for homooskedasticity with the Goldfeld-Quandt Test.
#'
#' @param model model
#' @param vars name of variable - the observations in the model are ordered by the values of this variable. If NULL - first variable is taken.
#'
#' @importFrom dplyr arrange_
#' @importFrom stats update rstandard predict
#'
#' @export

testGQ <- function(model, vars = NULL){
  dataFromModel <- model.frame(model)
  dependentVariable <- dataFromModel[,1]
  if (is.null(vars))  vars <- colnames(dataFromModel)[-1]


  p <- ncol(dataFromModel) - 1
  n <- nrow(dataFromModel)
  n1 <- n%/%2
  n2 <- n - n1

  df <- c(n2 - p - 1, n1 - p - 1)
  names(df) <- c("df1", "df2")

  GQResults <- list()
  for(variable in vars){
    dataFromModelOrdered <- arrange_(dataFromModel, variable)
    firstModelData <- dataFromModelOrdered[1:n1, ]
    secondModelData <- dataFromModelOrdered[(n1+1):n, ]
    firstModel <- update(model, data = firstModelData)
    secondModel <- update(model, data = secondModelData)
    firstModelResiduals <- getResiduals(firstModel, firstModelData[, 1])
    RSSA <- sum(firstModelResiduals^2)
    secondModelResiduals <- getResiduals(secondModel, secondModelData[, 1])
    RSSB <- sum(secondModelResiduals^2)

    firstModelStdResiduals <- getStdResiduals(firstModel, firstModelData[, 1])
    secondModelStdResiduals <- getStdResiduals(secondModel, secondModelData[, 1])
    statistic <- (RSSB/(n2 - p - 1)) / (RSSA/(n1 - p - 1))
    pValue = (2 * min(pf(statistic, df[1], df[2]), pf(statistic, df[1], df[2], lower.tail = FALSE)))
    values = list(firstModelData[ ,variable], secondModelData[ ,variable])
    residuals = list(firstModelResiduals, secondModelResiduals)
    residualsStd <- list(firstModelStdResiduals, secondModelStdResiduals)
    GQResults[[variable]] <- list(statistic = statistic, pValue = pValue, values = values, residuals=residuals, residualsStd = residualsStd)
  }

  result <- list(
    name = "Goldfeld-Quandt",
    GQResults = GQResults,
    parameter = df
  )
  class(result) <- "testAudit"
return(result)
}




