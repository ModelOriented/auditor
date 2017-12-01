#' @title Print ModelAudit object
#'
#' @description \code{print.ModelAudit} prints its argument.
#'
#' @param x an objecy of class ModelAudit.
#' @param ... further arguments passed to or from other methods.
#'
#' @export
print.modelAudit <- function(x, ...){
  print(x$model$call)
}

#' @title summary of a ModelAudit object
#'
#' @description Function \code{summary.ModelAudit} summarizes object of class ModelAudit.
#'
#' @param object an object of class ModelAudit.
#' @param ... further arguments passed to or from other methods.
#'
#' @export
summary.modelAudit <- function(object, ...){
  print(object, ...)
}
