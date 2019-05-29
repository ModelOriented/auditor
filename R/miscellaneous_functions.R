orderResidualsDF <- function(object, variable, is.df = FALSE, std.residuals = FALSE, type = "residuals"){
  if (std.residuals == TRUE)  type = "std.residuals"


  tmpDF <- data.frame(residuals = switch(type,
                                         residuals = {object$residuals},
                                         std.residuals = {object$std.residuals},
                                         y = {object$y},
                                         fitted.values = {object$fitted.values}
                                        )
                      )

  if (is.null(variable)) {
    values <- object$y
  } else if (variable == "") {
    values <- 1:nrow(object$data)
  } else {
    values <- object$data[,variable]
  }

  tmpDF$values <- values
  tmpDF$index <- rownames(object$data)
  tmpDF <- tmpDF[order(values), ]
  if(is.df == FALSE){
    return(tmpDF$residuals)
  } else {
    return(tmpDF)
  }

}
