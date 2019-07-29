orderResidualsDF <- function(object, variable, is.df = FALSE, std_residuals = FALSE, type = "residuals"){
  if (std_residuals == TRUE)  type = "std_residuals"

  tmpDF <- data.frame(residuals = switch(type,
                                         residuals = {object$residuals},
                                         std_residuals = {object$std_residuals},
                                         y = {object$y},
                                         fitted_values = {object$fitted_values}
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
