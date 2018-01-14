#' @title Half-Normal plot
#'
#' @description Function \code{plotHalfNormal}...
#'
#' @param object fitted model object or numeric vector
#' @param score if TRUE score based on probability density function
#' @param quant.scale if TRUE values on avis are on quantile scale
#' @param main title of plot
#' @param xlab the text for the x axis
#' @param ylab the text for the y axis
#' @param ... extra arguments passed to \link[hnp]{hnp}.
#'
#' @return An object of class ggplot
#'
#' @importFrom hnp hnp
#' @importFrom ggplot2 ggplot aes geom_point geom_line xlab ylab annotate scale_x_continuous scale_y_continuous ggtitle
#'
#' @export

plotHalfNormal <- function(object, score=TRUE, quant.scale=FALSE,
                           xlab = "Half-Normal Quantiles", ylab = "Residuals",
                           main = "", ...){
  x <- residuals <- upper <- lower <- NULL
  hnpObject <- halfNormal(object,...)

  dataPlot <- datasetHalfNormalPlot(hnpObject, quant.scale)

  p <- ggplot(dataPlot, aes(x = x)) +
    geom_point(aes(y = residuals)) +
    geom_line(aes(y=upper))+
    geom_line(aes(y=lower))+
    geom_line(aes(y=median), linetype = 2) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(main) +
    theme_classic()

  if(score==TRUE) {
    envScore <- calculateScorePDF(hnpObject)
    p <- p + annotate("text", x = max(dataPlot$x)/4, y = max(dataPlot$residuals)*3/4, label = paste("Score:",round(envScore,2)))
  }
  return(p)
}

#' Calculating simulated residuals and envelope
#' @usage NULL
#' @importFrom graphics plot
#' @importFrom stats density qnorm quantile
#' @importFrom utils assignInNamespace
halfNormal <- function(object, ...){
  #.makehnp2 is modified .makehnp function form hnp package
  .makehnp2 <- function(obj, conf, halfnormal, how.many.out, paint.out, col.paint.out, print.on, plot.sim, ...) {
    # A few checks
    if(print.on) how.many.out <- T
    if(paint.out) {
      how.many.out <- T
      if(missing(col.paint.out)) col.paint.out <- 2
    }
    # Residuals and simulated envelope
    res.original <- obj[,1]
    res <- obj[,-1]
    env <- apply(res, 1, quantile, c((1-conf)/2, .5, (1-conf)/2+conf))
    # Saving / plotting
    n <- nrow(res)
    i <- 1:n
    if(halfnormal) q.x <- qnorm((i+n-1/8)/(2*n+1/2)) else q.x <- qnorm((i-3/8)/(n+1/4))
    simdata <- list(q.x, t(env)[,1], t(env)[,2], t(env)[,3], res.original, res)
    class(simdata) <- "hnp"
    names(simdata) <- c("x", "lower", "median", "upper", "residuals","simresiduals")
    if(how.many.out) {
      mat <- cbind(t(env), res.original, q.x)
      out <- sum(mat[,4] > mat[,3] | mat[,4] < mat[,1])
      if(paint.out) {
        simdata$out.index <- matrix(mat[mat[,4] > mat[,3] | mat[,4] < mat[,1], 4:5], ncol=2)
        simdata$col.paint.out <- col.paint.out
      }
      simdata$how.many.out <- TRUE
      simdata$total <- nrow(mat)
      simdata$out <- out
      simdata$print.on <- print.on
      simdata$paint.out <- paint.out
    } else {
      simdata$how.many.out <- FALSE
    }
    if(plot.sim) {
      plot(simdata, ...)
      return(invisible(simdata))
    } else {
      return(simdata)
    }
  }

  tmpfun <- get(".makehnp", envir = asNamespace("hnp"))
  environment(.makehnp2) <- environment(tmpfun)
  attributes(.makehnp2) <- attributes(tmpfun)  # don't know if this is really needed
  assignInNamespace(".makehnp", .makehnp2, ns="hnp")

  hnpObject <- hnp(object, plot.sim=FALSE, ...)
}




#' Creating dataset for Half-Normal Plot
#' @usage NULL
#' @importFrom fdrtool phalfnorm
datasetHalfNormalPlot <- function(hnpObject, quant.scale){
  n <- length(hnpObject$residuals)

  if (quant.scale == FALSE) {
    dataPlot <- data.frame(x = hnpObject$x, lower = hnpObject$lower,
                           median = hnpObject$median, upper = hnpObject$upper,
                           residuals = hnpObject$residuals)
  } else {
    quantilesResiduals <-  seq(0,1,length.out = n)
    quantilesTheoretical <- phalfnorm(hnpObject$residuals)
    quantilesUpper <- phalfnorm(hnpObject$upper)
    quantilesMedian <- phalfnorm(hnpObject$median)
    quantilesLower <- phalfnorm(hnpObject$lower)
    dataPlot <- data.frame(x = quantilesTheoretical, lower = quantilesLower,
                           median = quantilesMedian, upper = quantilesUpper,
                           residuals = quantilesResiduals)
  }
  return(dataPlot)
}

#' Calculating Liklehood for each residual
#' @usage NULL
#' @importFrom stats dnorm density
calculateKDE <- function(res, simres){
  h <- density(simres)$bw
  n <- length(simres)
  kernelValues <- rep(0,n)
  for(i in 1:n){
    transformed = (res - simres[i]) / h
    kernelValues[i] <- dnorm(transformed, mean = 0, sd = 1) / h
  }
  return(sum(kernelValues) / n)
}


#' Calculating PDF score
#' @usage NULL
calculateScorePDF <- function(hnpObject){
  res <- hnpObject$residuals
  simres<-as.data.frame(hnpObject$simresiduals)
  n <- length(res)
  PDFs <- mapply(calculateKDE, res[1:n], simres[ ,1:n])
  return(sum(PDFs))
}





