#' @title Half-Normal plot
#'
#' @description The half-normal plot is one of the tools designed to evaluate the goodness of fit of a
#' statistical models. It is a graphical method for comparing two probability distributions by plotting
#' their quantiles against each other.
#' Points on the plot correspond to ordered absolute values of model diagnostic
#' (i.e. standardized residuals) plotted against theoretical order statistics from a half-normal distribution.
#'
#' @param object modelAudit object, modelFit object.
#' @param quantiles if TRUE values on axis are on quantile scale.
#' @param ... extra arguments passed to \link[hnp]{hnp}.
#'
#' @return An object of class ggplot
#'
#' @import ggplot2
#' @importFrom hnp hnp
#' @importFrom fdrtool phalfnorm
#' @importFrom stats ecdf dnorm density
#'
#' @seealso \code{\link{scoreHalfNormal}}
#'
#' @export

plotHalfNormal <- function(object, quantiles = FALSE, ...) {

  # some safeguard
  x <- residuals <- upper <- lower <- NULL

  # data frame for ggplot object
  df <- make_dataframe(object, ..., quant = quantiles, type = "fit")


  # main chart
  p <- ggplot(data = df, aes(x)) +
    geom_point(aes(y = residuals), colour = "#371ea3") +
    geom_line(aes(y = upper)) +
    geom_line(aes(y = lower)) +
    geom_line(aes(y = median), linetype = 2, colour = "darkgrey")

  # theme, colours, titles, axes, scales, etc.
  p <- p + theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3")) +
    xlab("Half-normal Quantiles") +
    ylab("Residuals") +
    ggtitle("Podaj tytul")

  if (quantiles == TRUE) {
    p + scale_x_continuous(limits = c(0, 1), breaks = scales::pretty_breaks()) +
      scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks()) +
      coord_fixed(ratio = 1)

  } else {
    p + scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks()) +
      scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks())

  }

  return(p)
}

# Calculating Likelihood for each residual
calculateKDE <- function(res, simres){
  simres <- as.numeric(simres)
  (abs(sum(res<=simres) - length(simres)/2))/(length(simres)/2)
}


# Calculating PDF score
calculateScorePDF <- function(hnpObject){
  res <- hnpObject$residuals
  simres <- as.data.frame(t(hnpObject[,6:ncol(hnpObject)]))
  n <- length(res)
  PDFs <- mapply(calculateKDE, res, simres)
  return(sum(PDFs))
}


