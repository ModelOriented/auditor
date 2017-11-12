#' @title Getting test name, statistic value and p.value for chosen test from ModelAudit object.
#'
#' @description a
#'
#' @param x element of ModelAudit object
#' @param name name of assumption

collect_assumption <- function(x, name){
  if(typeof(x) != "list" || is.null(x$assumption) || x$assumption != name) return(NULL)
  values <- c(x$name, x$statistic, x$p.value)
  return(values)
}


#' @title printing results of tests for chosen assumption
#'
#' @description a
#'
#' @param ModelAudit object of class ModelAudit
#' @param assumption name of assumption
#' @param digits the minimum number of significant digits to be printed in values
#'
cat_assumption <- function(ModelAudit, assumption, digits = 3){
results <- lapply(ModelAudit, function(x) collect_assumption(x, name = assumption))
results <- results[sapply(results, function(x) !is.null(x))]
results <- data.frame(t(data.frame(results)))
colnames(results) <- c("Test", "statistic", "p.value")
rownames(results) <- NULL
results$p.value <- as.numeric(as.character(results$p.value))
results$statistic <- as.numeric(as.character(results$statistic))
cat(assumption, "\n")
print(results, row.names = FALSE, digits = digits)
}


#####
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

