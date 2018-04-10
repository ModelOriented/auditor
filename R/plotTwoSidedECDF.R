#' @title Two-sided Cumulative Distribution Function
#'
#' @description Two-sided Cumulative Distribution Function
#'
#' @param object An object of class ModelAudit
#' @param ... other modelAudit objects to be plotted together
#' @param error.scaled Should ECDF be scaled by proportions of positive and negative proportions?
#' @param outliers number of outliers to be marked
#' @param residuals should residuals be marked?
#' @param y.reversed should values on y axib be reversed
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @importFrom plyr ddply
#'
#' @export


plotTwoSidedECDF <- function(object, ..., error.scaled = TRUE, outliers = NA,
                             residuals = TRUE, y.reversed = FALSE){
  res <- ecd <- label <- big <- no.obs <- NULL
  df <- getTwoSidedECDF(object, error.scaled, outliers, y.reversed)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- rbind( df, getTwoSidedECDF(resp, error.scaled, outliers, y.reversed ) )
      }
    }
  }


  p <- ggplot(df, aes(x = res, y = ecd, color = label)) +
    geom_step() +
    theme_light() +
    scale_y_continuous(breaks = seq(0,1,0.1),
                       labels = paste(seq(0, 100, 10),"%"),
                       name = "") +
    xlab("residuals")

  if (residuals == TRUE) {
    p <- p +
      geom_point( aes(x = res, y = ecd, color = label), size = 2) +
      geom_text_repel(data = subset(df, big==TRUE), aes(label=as.character(no.obs)),
                      show.legend = FALSE, direction = "y", color="black")
  }

  return(p)
}


getTwoSidedECDF <- function(object, error.scaled, outliers, y.reversed){
  res <- object$residuals
  resids <- data.frame(no.obs = 1:(length(res)), res=res, sign = ifelse(res>=0, "pos", "neg"))

  df <- ddply(resids, "sign", transform, ecd = ecdf(res)(res))

  if (y.reversed == FALSE){
    df$ecd <- ifelse(df$sign == "neg", 1 - df$ecd, df$ecd)
  } else {
    df$ecd <- ifelse(df$sign == "neg", df$ecd, 1 - df$ecd)
  }

  if (error.scaled == TRUE) {
    negProportion <- sum(df$sign == "neg") / (sum(df$sign == "neg") + sum(df$sign == "pos"))
    posProportion <- 1 - negProportion

    df$ecd <- ifelse(df$sign == "neg", df$ecd * negProportion, df$ecd * posProportion)
  }

  if (!is.na(outliers)) {
    df <- dplyr::arrange(df, res)
    df$big <- c(rep(TRUE, outliers), rep(FALSE, nrow(object$data) - 2 * outliers), rep(TRUE, outliers))
  } else {
    df$big <- FALSE
  }

  df$label = object$label
  return(df)
}



