#' @title Cooks distance plot
#'
#' @description cooks distances
#'
#'
#' @param object An object of class ModelAudit
#' @param cut.off horizontal line
#'
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggplot2 geom_point geom_hline geom_text
#' @importFrom ggplot2 ggtitle xlab ylab theme
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 element_blank
#'
#' @export

plot_cooks_dist <- function(object, cut.off = 1){
  index <- cooks.dist <- NULL
  cooks.distances <- data.frame(cooks.dist = object$cooks.dist, index = names(object$cooks.dist))

  ggplot(cooks.distances, aes(index, cooks.dist)) +
    geom_hline(yintercept = cut.off) +
    geom_point() +
    geom_text(aes(label=ifelse(cooks.dist > cut.off, as.character(index), '')),hjust=0,vjust=0, col = "red") +
    xlab("observation index") +
    ylab("cooks distance") +
    ggtitle("Influence of observations") +
    theme_classic() +
    theme(axis.text.x = element_blank())

}

