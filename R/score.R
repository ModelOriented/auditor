#' @title Model scores computations
#'
#' @description This function provides many scores
#'
#' @param object object An object of class ModelAudit
#' @param score the type of score to  be calculated. Possible values: 'GQ', 'DW', 'Runs', 'HalfNormal', 'Cook' (see the details section).
#' @param ... other arguments dependent on the type of score.
#'
#' @details TODO: desription of scores
#'
#' @seealso \code{\link{scoreGQ}, \link{scoreDW}, \link{scoreRuns}, \link{scoreHalfNormal}, \link{scoreCook}}
#'
#' @export

score <- function(object, score = 'GQ', ...){

  if(class(object)!="modelAudit") stop( paste0(deparse(substitute(object)), ' not of class modelAudit. Try to use audit(', deparse(substitute(object)), ') first.'))

  switch(score,
         GQ={ return(scoreGQ(object, ...)) },
         DW={ return(scoreDW(object, ...)) },
         Runs={ return(scoreRuns(object, ...)) },
         HalfNormal={ return(scoreHalfNormal(object, ...)) },
         Cook = { return(scoreCook(object, ...)) })
  stop( "Wrong type of score. Possible values: 'GQ', 'DW', 'Runs', 'HalfNormal', 'Cook'." )
}
