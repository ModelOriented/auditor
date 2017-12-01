#' @title plot
#'
#' @description plot
#'
#' @param x object of class ModelAudit
#' @param variable name of variable to order residuals
#' @param ... further arguments passed to or from other methods.
#'
#' @importFrom ggpubr ggarrange
#'
#' @export

plot.modelAudit <- function(x, variable, ...){
  test_gq <- plotTestGQ(x, variable)
  autocorr <- plotAutocorr(x, variable)
  ACF <- plotACF(x, variable)
  ggarrange(test_gq, autocorr, ACF)
}
