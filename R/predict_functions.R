getPredictFunction <- function(model, predict.function){
  data <- NULL
  if(is.null(predict.function)){
     predict.function <-  switch(class(model)[1],
                              lm = stats::predict,
                              glm = function(model, data){predict(model, data, type="response")},
                              randomForest.formula = predictRandomForest(model, data),
                              stop(cat("No default prediction function for model of class",paste0(class(model),".")," Please provide it by predict.function argument."))
                          )

   }
  return(predict.function)
}


predictRandomForest <- function(model, data){
  if (model$type == "classification") {
    predict.function <- function(model, data){stats::predict(model, data, type="prob")[,2]}
  } else {
    predict.function <- function(model, data){stats::predict(model, data, type="response")}
  }
  return(predict.function)
}
