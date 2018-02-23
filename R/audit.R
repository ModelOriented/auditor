#' @title audit
#'
#' @description Function \code{audit} create ModelAudit object for further validation of model.
#'
#' @param model An object of appropriate class containing a model
#' @param data data.frame or matrix - data used for fitting. If not provided, will be extracted from the model.
#' @param y response used for building a model
#'
#' @return An object of class ModelAudit.
#'
#' @importFrom stats model.frame
#'
#' @export



audit <- function(model, data=NULL, y = NULL){
  dataModel <- auditError(model, data, y)

  result <- list(
    model.class = class(model),
    model = model,
    fitted.values = predict(model, type = "response", newdata = dataModel$data),
    data = dataModel$data,
    y = dataModel$y,
    residuals = getResiduals(model, dataModel$y, dataModel$data),
    std.residuals = getStdResiduals(model, dataModel$y, dataModel$data)
  )
  class(result) <- "modelAudit"
  return(result)
}

auditError <- function(model, data, y){
  if (is.null(data)) {
    possibleData <- try(model.frame(model), silent = TRUE)
    if (class(possibleData) != "try-error") {
      data <- possibleData
      y <- data[, 1]
    } else { stop("data cannot be extracted from the model. The audit() function requires specified 'data' parameter.")
    }
  }
  if (is.null(y)) {stop("Original response cannot be extracted from the model. The audit() function requires specified 'y' parameter.") }
  return(list(data = data, y = y))
}
