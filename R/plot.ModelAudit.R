#' @title plot
#'
#' @description plot
#'
#' @param x object of class ModelAudit
#' @param ... further arguments passed to or from other methods.
#'
#' @importFrom ggpubr ggarrange
#'
#' @export

plot.ModelAudit <- function(x, ...){
  plot(x=c(1,2), y=c(2,1), main = "Nothing here")
}
