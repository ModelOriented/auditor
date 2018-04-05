#' @title Two-sided Cumulative Distribution Function
#'
#' @description Two-sided Cumulative Distribution Function
#'
#' @param object An object of class ModelAudit
#' @param ... other modelAudit objects to be plotted together
#' @param error.scale Should ECDF be scaled by proportions of positive and negative proportions?
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#' @importFrom plyr ddply
#'
#' @export


plotTwoSidedECDF <- function(object, ..., error.scaled = TRUE){
  RROCX <- res <- ecd <- label <- NULL
  df <- getTwoSidedECDF(object, error.scaled)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- rbind( df, getTwoSidedECDF(resp, error.scaled) )
      }
    }
  }
  maxPos <- max(df$res[which(df$sign=="pos")])
  minNeg <- min(df$res[which(df$sign=="neg")])
  for(lab in unique(df$label)){
    maxNegECDF <- max(df$ecd[which(df$sign=="neg" & df$label == lab)])
    df <- rbind(df, data.frame(res = as.numeric(minNeg), sign = "neg", ecd = as.numeric(maxNegECDF), label = lab))
    maxPosECDF <- max(df$ecd[which(df$sign=="pos" & df$label == lab)])
    df <- rbind(df, data.frame(res = as.numeric(maxPos), sign = "pos", ecd = as.numeric(maxPosECDF), label = lab))
  }


  ggplot(df, aes(x = res, y = ecd, color = label)) +
    geom_step() +
    theme_light() +
    scale_y_continuous(breaks = seq(0,1,0.1),
                       labels = paste(seq(0, 100, 10),"%"),
                       name = "") +
    xlab("residuals")

}


getTwoSidedECDF <- function(object, error.scaled){
  res <- object$residuals
  resids <- data.frame(res=res, sign = ifelse(res>=0, "pos", "neg"))

  df <- ddply(resids, .(sign), transform, ecd = ecdf(res)(res))
  df$ecd <- ifelse(df$sign == "neg", 1 - df$ecd, df$ecd)

  if (error.scaled == TRUE) {
    negProportion <- sum(df$sign == "neg") / (sum(df$sign == "neg") + sum(df$sign == "pos"))
    posProportion <- 1 - negProportion

    df$ecd <- ifelse(df$sign == "neg", df$ecd * negProportion, df$ecd * posProportion)
  }

  df$label = object$label
  return(df)
}



