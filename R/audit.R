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
  nColumns <- ncol(dataFromModel)
  result <- list(
    model = model,
    fitted.values = predict(model),
    data = dataFromModel,
    indep.var = colnames(dataFromModel)[1],
    dep.var =colnames(dataFromModel)[2:nColumns],
    residuals = getResiduals(model, dataFromModel[, 1]),
    std.residuals = getStdResiduals(model, dataFromModel[, 1])
  )
  class(result) <- "modelAudit"
  return(result)
}


