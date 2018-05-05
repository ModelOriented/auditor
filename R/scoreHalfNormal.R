#' @title Half-Normal Score
#'
#' @description Simulated score
#'
#' @param object ModelAudit object or fitted model
#' @param ... extra arguments passed to \link[hnp]{hnp}.
#'
#' @importFrom hnp hnp
#'
#' @export

scoreHalfNormal <- function(object, ...){
  if(class(object)=="modelAudit") object <- object$model

  hnpObject <- halfNormal(object,...)
  result <- list(
    name = "halfNormal",
    score = calculateScorePDF(hnpObject)
  )

  class(result) <- "scoreAudit"
  return(result)
}



# calculateScorePDF fucntion is in plotHalfNormal.R file
