order_residuals <- function(object, variable){

  ordered_df <- object

  if (is.null(variable)) {
    ordered_df$`_values_` <- object$y
  } else if (variable == "") {
    ordered_df$`_values_` <- 1:nrow(object$data)
  } else {
    ordered_df$`_values_` <- object$data[ ,variable]
  }

  ordered_df <- ordered_df[order(ordered_df$`_values_`), ]

  ordered_df
}
