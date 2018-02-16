#' @title audit
#'
#' @description Function \code{audit} create ModelAudit object for further validation of model.
#'
#' @param model An object of appropriate class containing a model
#'
#' @return An object of class ModelAudit.
#'
#' @importFrom stats model.frame
#'
#' @export



audit <- function(model){
  dataFromModel <- model.frame(model)
  result <- list(
    model = model,
    data = dataFromModel,
    residuals = getResiduals(model, dataFromModel[, 1]),
    std.residuals = getStdResiduals(model, dataFromModel[, 1])
  )
  class(result) <- "modelAudit"
  return(result)
}


