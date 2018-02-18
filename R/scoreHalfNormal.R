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
  res <- calculateScorePDF(hnpObject)
  return(res)
}



# calculateScorePDF fucntion is in plotHalfNormal.R file
