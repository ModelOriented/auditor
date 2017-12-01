ordered_model <- function(model, variable = NULL){
  V2 <- NULL
  model.data <-  augment(model)
  residuals <- model.data$.resid
  if(is.null(variable)) variable <- colnames(model.data)[3]

  vals <- as.data.frame(cbind(residuals, model.data[ ,c(variable)]))
  vals <- arrange(vals, V2)
  colnames(vals) <- c("residuals", variable)
  return(vals)
}

