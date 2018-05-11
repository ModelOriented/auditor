#' @title Goldfeld-Quandt Score
#'
#' @description This score is calculated on the basis of Goldfeld-Quandt test, which is used for checking for homoscedasticity of residuals in regression analyses.
#'
#' Goldfeld-Quandt test is based on a comparison of the residual sum of squares (RSS) using the F-statistic.
#' The test consists of dividing dataset into two groups. Then estimating models separately for each subset and calculating
#' the residual sum of squares for each group (RSS1 and RSS2).
#'
#' The test statistic is the ratio of the mean square residual errors for two groups, which corresponds to the F-test of equality of variances.
#' \eqn{F = (MSE_1)/(MSE_2)}
#' where \eqn{MSE = (RSS)/(n-p)}
#' where n is the number of observations and p is the number of variables .
#'
#' The score value is helpful in comparing models. It is worth pointing out that results of tests like p-value makes sense only
#' when the test assumptions are satisfied. Otherwise test statistic may be considered as a score.
#' \code{scoreGQ} function uses a two-sided F-test.
#'
#' @param object Object An object of class ModelAudit.
#' @param variable Name of model variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' scoreGQ(lm_au)
#'
#' @importFrom stats update rstandard predict pf sd
#'
#' @return an object of class scoreAudit
#'
#' @export

scoreGQ <- function(object, variable = NULL){

  dataForModels <- getOrderedData(object, variable)
  originalModel <- object$model
  residual.function <- object$residual.function

  p <- ncol(dataForModels) - 1
  n <- nrow(dataForModels)
  n1 <- n%/%2
  n2 <- n - n1

  df <- c(n2 - p - 1, n1 - p - 1)
  names(df) <- c("df1", "df2")

    firstModelData <- dataForModels[1:n1, ]
    secondModelData <- dataForModels[(n1+1):n, ]
    firstModel <- update(originalModel, data = firstModelData)
    secondModel <- update(originalModel, data = secondModelData)
    firstModelResiduals <- residual.function(firstModel, firstModelData, firstModelData[, 1],  object$predict.function)
    RSSA <- sum(firstModelResiduals^2)
    secondModelResiduals <- residual.function(secondModel, secondModelData, secondModelData[, 1], object$predict.function)
    RSSB <- sum(secondModelResiduals^2)

    firstModelStdResiduals <- firstModelResiduals/sd(firstModelResiduals)
    secondModelStdResiduals <- secondModelResiduals / sd(secondModelResiduals)
    statistic <- (RSSB/(n2 - p - 1)) / (RSSA/(n1 - p - 1))
    pValue = (2 * min(pf(statistic, df[1], df[2]), pf(statistic, df[1], df[2], lower.tail = FALSE)))

    GQResults <- list(
      name = "Goldfeld-Quandt",
      score = statistic,
      parameter = df,
      pValue = pValue)

  class(GQResults) <- "scoreAudit"
  return(GQResults)
}


getOrderedData <- function(object, variable){
  dataFromModel <- object$data

  if(!is.null(variable)){
    if((variable == "Predicted response") || (variable == "Fitted values")) {
      values <- object$fitted.values
    } else {
      values <- object$data[,variable]
    }
    dataFromModel <- dataFromModel[order(values), ]
  }

    return(dataFromModel)
}



