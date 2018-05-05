#' @title Model diagnostic plots
#'
#' @description This function provides several diagnostic plots for regression and classification models.
#'
#' @param x object of class modelAudit
#' @param ... other arguments dependent on the type of plot or additionam objects of class modelAudit
#' @param type the type of plot. Possible values: 'ACF', 'Autocorrelation', 'CGains', 'Cook', 'HalfNormal', 'Residuals', 'LIFT',
#' ModelPCA', 'Pairs', 'Prediction', 'REC', 'ResidDens', 'Residuals', 'ROC', 'RROC',
#' ScaleLocation', 'TwoSidedECDF' (for detailed description see functions in seealso section).
#' @param ask logical; if TRUE, the user is asked before each plot, see \code{\link[graphics]{par}(ask=)}.
#'
#' @seealso \code{\link{plotACF}, \link{plotAutocorrelation}, \link{plotCGains}, \link{plotCook},
#' \link{plotHalfNormal}, \link{plotResiduals}, \link{plotLIFT}, \link{plotModelPCA}, \link{plotPairs},
#' \link{plotPrediction}, \link{plotREC}, \link{plotResidDens}, \link{plotResiduals}, \link{plotROC},
#' \link{plotRROC}, \link{plotScaleLocation}, \link{plotTwoSidedECDF}}
#'
#' @importFrom grDevices devAskNewPage
#'
#' @method plot modelAudit
#'
#' @export

plot.modelAudit <- function(x, ..., type="Residuals", ask = TRUE){

  object <- x

  plotNames <- c('ACF', 'Autocorrelation', 'CGains', 'Cook', 'HalfNormal', 'Residuals', 'LIFT',
                 'ModelPCA', 'Pairs', 'Prediction', 'REC', 'ResidDens', 'Residuals', 'ROC', 'RROC',
                 'ScaleLocation', 'TwoSidedECDF')

  if(!all(type %in% plotNames)){
    stop(paste0("Invalid plot type. Possible values are: ", paste(plotNames, collapse = ", "),"."))
  }

  if (length(type)==1) {
    return(plotTypePlot(object, ..., type = type))
  }

  if (ask & length(type)) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }

  plotsList <- sapply(type, function(x) NULL)

  for(name in type){
    plotsList[[name]] <- plotTypePlot(object, ..., type = name)
    plot(plotsList[[name]])
  }
  class(plotsList) <- c("auditorPlotList", "list")
  return(plotsList)
}

plotTypePlot <- function(x, ..., type){
  switch(type,
         ACF = { return(plotACF(x, ...)) },
         Autocorrelation = { return(plotAutocorrelation(x, ...)) },
         CGains = {return(plotCGains(x, ...))},
         Cook = { return(plotCook(x, ...)) },
         HalfNormal = { return(plotHalfNormal(x, ...)) },
         LIFT = {return(plotLIFT(x, ...))},
         ModelPCA = {return(plotModelPCA(x, ...))},
         Pairs = {return(plotPairs(x, ...))},
         Prediction = {return(plotPrediction(x, ...))},
         REC = { return(plotREC(x, ...)) },
         ResidDens = { return(plotResidDens(x, ...)) },
         Residuals = { return(plotResiduals(x, ...)) },
         ROC = { return(plotROC(x, ...)) },
         RROC = { return(plotRROC(x, ...)) },
         ScaleLocation = { return(plotScaleLocation(x, ...)) },
         TwoSidedECDF = { return(plotTwoSidedECDF(x, ...)) }
  )
}
