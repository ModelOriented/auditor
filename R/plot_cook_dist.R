#' @title Cooks distance plot
#'
#' @description cooks distances
#'
#'
#' @param object An object of class ModelAudit
#'
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggplot2 geom_point geom_hline
#' @importFrom ggplot2 ggtitle xlab ylab
#' @importFrom ggplot2 theme_classic
#'
#' @export

plot_cooks_dist <- function(object, cut.off = 1){
  cooks.distances <- data.frame(cooks.dist = object$cooks.dist, index = names(object$cooks.dist))
  cut.off <- ifelse(cut.off < max(object$cooks.dist), cut.off, 0)

  ggplot(cooks.distances, aes(index, cooks.dist)) +
    geom_hline(yintercept = cut.off) +
    geom_point() +
    xlab("observation index") +
    ylab("cooks distance") +
    ggtitle("Influence of observations") +
    geom_text(aes(label=ifelse(cooks.dist>cut.off,as.character(Index),'')),hjust=0,vjust=0, col = "red") +
    theme_classic() +
    theme(axis.text.x = element_blank())

}
