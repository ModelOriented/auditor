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
  test_gq <- plot_test_gq(x)
  autocorr <- plot_autocorr(x)
  ACF <- plot_acf(x)
  ggarrange(test_gq, autocorr, ACF, ncol = 1)
}
