orderResidualsDF <- function(object, variable, is.df = FALSE){
  tmpDF <- data.frame(residuals=object$residuals)
  if(!is.null(variable)){
    if((variable == "Predicted response") || (variable == "Fitted values")) {
      values <- object$fitted.values

    } else if (variable == "Observed response") {
      values <- object$y
    } else {
      values <- object$data[,variable]
    }
    tmpDF$values <- values
    tmpDF <- tmpDF[order(values), ]
  }
  if(isFALSE(is.df)){
    return(tmpDF$residuals)
  } else {
    return(tmpDF)
  }

}
