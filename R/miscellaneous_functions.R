orderResidualsDF <- function(object, variable, is.df = FALSE, std.residuals = FALSE, type = "residuals"){
  if (std.residuals == TRUE)  type = "std.residuals"


  tmpDF <- data.frame(residuals = switch(type,
                                         residuals = {object$residuals},
                                         std.residuals = {object$std.residuals},
                                         y = {object$y},
                                         fitted.values = {object$fitted.values}
                                        )
                      )

  if(!is.null(variable)){
    if((variable == "Predicted response") || (variable == "Fitted values")) {
      values <- object$fitted.values
    } else if (variable == "Observed response") {
      values <- object$y
    } else {
      values <- object$data[,variable]
    }

  } else {
    values <- 1:nrow(tmpDF)
  }

  tmpDF$values <- values
  tmpDF <- tmpDF[order(values), ]
  if(is.df == FALSE){
    return(tmpDF$residuals)
  } else {
    return(tmpDF)
  }

}
