#' @title Scale location plot
#'
#' @description Variable values vs square root of the absolute value of the residuals.
#' A vertical line corresponds to median.
#' Goldfeld-Quandt test - checking assumption of homoscedasticity of residuals.
#'
#'
#' @param object An object of class ModelAudit
#' @param variable name of variable to order residuals
#'
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggplot2 geom_point geom_smooth geom_vline
#' @importFrom ggplot2 ggtitle xlab ylab
#' @importFrom ggplot2 theme_classic
#' @importFrom stats median
#'
#' @export
plotTestGQ <- function(object, variable){
  values <- sqrt.std.residuals <- NULL

  df <- generate_test_gq_df(object, variable)


  ggplot(df, aes(x = values, y = sqrt.std.residuals)) +
    geom_vline(aes(xintercept = median(df$values))) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE) +
    xlab(variable) +
    ylab("\u221A|Standarized residuals|") +
    ggtitle("Scale Location") +
    theme_classic()
}

generate_test_gq_df <- function(object, variable){

  std.residuals1 = object$testGQ$GQResults[[variable]]$residualsStd[[1]]
  std.residuals2 = object$testGQ$GQResults[[variable]]$residualsStd[[2]]
  values1 = object$testGQ$GQResults[[variable]]$values[[1]]
  values2 = object$testGQ$GQResults[[variable]]$values[[2]]

  df1 <- data.frame(std.residuals = std.residuals1,
                    values = values1,
                    group = rep("<med", length(values1)))
  df2 <- data.frame(std.residuals = std.residuals2,
                    values = values2,
                    group = rep(">med", length(values2)))
  df <- rbind(df1, df2)
  df$sqrt.std.residuals <- sqrt(abs(df$std.residuals))
  return(df)
}
