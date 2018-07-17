#' @title Residual Density Plot
#'
#' @description Density of model residuals.
#'
#' @param object An object of class ModelAudit.
#' @param ... Other modelAudit objects to be plotted together.
#' @param split.var Logical. Indicates whenever plot should be splitted by variable.
#' @param variable variable name o split. Optional. Should be provided  only for modelAudit object.
#'
#' @return ggplot object
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' plotResidualDensity(lm_au)
#'
#' library(randomForest)
#' rf_model <- randomForest(prestige~education + women + income, data = Prestige)
#' rf_au <- audit(rf_model, data = Prestige, y = Prestige$prestige)
#' plotResidualDensity(lm_au, rf_au)
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#'
#' @export


plotResidualDensity <- function(object, ..., split.var = TRUE, variable = NULL){
  if(!("modelResiduals" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelResiduals().")
  if(!("modelResiduals" %in% class(object))) object <- modelResiduals(object, variable)

  res <- label <- div <- NULL

  df <- getDivision(object)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
        if("modelAudit" %in% class(resp)) df <- rbind( df, getDivision(modelResiduals(resp, variable)) )
        if("modelResiduals" %in% class(resp)) df <- rbind( df, getDivision(resp) )
    }
  }

  variable <- df$variable[1]

  if(split.var == FALSE || is.na(variable)) {
    p <- ggplot(df, aes(x = res, fill = label)) +
      stat_density(alpha = 0.3, position="identity")+
      geom_vline(xintercept = 0) +
      geom_rug(aes(color = label), alpha = 0.5) +
      theme_light() +
      xlab("residuals") +
      ggtitle("Residual Density")
  } else {
    p <- ggplot(df, aes(x = res, fill = div)) +
      stat_density(alpha = 0.3, position="identity")+
      geom_vline(xintercept = 0) +
      geom_rug(aes(color = div), alpha = 0.5) +
      facet_grid(label~.) +
      theme_light() +
      xlab("residuals") +
      ggtitle("Residual Density")
  }

  return(p)
}


getDivision <- function(modelData){
    variable <- modelData$variable[1]
    df <- modelData
    if (class(modelData$val) %in% c("numeric","integer")) {
      varMedian <- median(modelData$val)
      df$div <- ifelse(modelData$val > varMedian, paste(">", variable, "median"), paste("<=", variable, "median"))
    } else {
      df$div <- modelData$val
    }

  return(df)
}

