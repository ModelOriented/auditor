#' @title VIF plot
#'
#' @description VIF (Variance inflation factor) computed for each variable
#'
#'
#' @param object An object of class ModelAudit
#' @param cut.off horizontal line
#'
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggplot2 geom_point geom_hline geom_text
#' @importFrom ggplot2 ggtitle xlab ylab
#' @importFrom ggplot2 theme_classic
#'
#' @export

plot_vif <- function(object, cut.off = 10){
  variable <- vif <- NULL
  variables_vif <- data.frame(vif = object$VIF, variable = names(object$VIF))

  ggplot(variables_vif, aes(variable, vif)) +
    geom_hline(yintercept = cut.off) +
    geom_point() +
    xlab("variable") +
    ylab("VIF") +
    ggtitle("Multicollinearity of variables") +
    geom_text(aes(label=ifelse(vif > cut.off, as.character(variable),'')),hjust=0,vjust=0, col = "red") +
    theme_classic()
}
