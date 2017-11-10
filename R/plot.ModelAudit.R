#' @title plot
#'
#' @description plot
#'
#' @param x object of class ModelAudit
#' @param ... further arguments passed to or from other methods.
#'
#'
#' @export

plot.ModelAudit <- function(x, ...){
  graphics::plot(x=c(1,2), y=c(2,1), main = "Nothing here")
}
