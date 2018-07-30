#' @title Model Scores computations
#'
#' @description This function provides several scores for model validation and performance assessment.
#' Scores can be also used to compare models.
#'
#' @param object Object An object of class modelAudit.
#' @param type The type of score to  be calculated. Possible values: 'Cook', 'DW', 'GQ', 'HalfNormal', 'MAE', 'MSE', 'REC', 'RMSE', 'ROC', 'RROC', 'Runs'
#' (for detailed description see functions in see also section).
#' @param ... Other arguments dependent on the type of score.
#'
#' @seealso \code{\link{scoreCooksDistance}, \link{scoreDW}, \link{scoreGQ}, \link{scoreHalfNormal}, \link{scoreMAE}, \link{scoreMSE}, \link{scoreREC}, \link{scoreROC}, \link{scoreRROC}, \link{scoreRuns}}
#'
#' @return an object of class scoreAudit, except Cooks distance, where numeric vector is returned
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' score(lm_au, type = 'Runs')
#'
#' @export

score <- function(object, type = 'MSE', ...){

  switch(type,
         CooksDistance = { return(scoreCooksDistance(object, ...)) },
         DW = { return(scoreDW(object, ...)) },
         GQ = { return(scoreGQ(object, ...)) },
         HalfNormal = { return(scoreHalfNormal(object, ...)) },
         MSE = { return(scoreMSE(object, ...)) },
         MAE = { return(scoreMAE(object, ...)) },
         REC = { return(scoreREC(object, ...)) },
         RMSE = { return(scoreRMSE(object, ...)) },
         ROC = { return(scoreROC(object, ...)) },
         RROC = { return(scoreRROC(object, ...)) },
         Runs = { return(scoreRuns(object, ...)) }
  )
  stop( "Wrong type of score. Possible values: 'CooksDistance', 'DW', 'GQ', 'HalfNormal', 'RMSE', 'MAE', 'MSE', 'REC', 'ROC', 'RROC', 'Runs'." )
}
