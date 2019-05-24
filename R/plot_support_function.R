
#' @title Check object
#'
#' @description Checks if the object is of desired class
#'
#' @param object Object passed to the function
#' @param type Type of check; default is \code{res} which stands for "model residuals".
#' Other possible values: \code{eva} - model evaluation
check_object <- function(object, type = "res") {
  model_type <- switch(type,
                       "res" = "modelResiduals",
                       "eva" = "modelEvaluation")

  if (!(model_type %in% class(object) || "modelAudit" %in% class(object))) {
    stop("The function requires an object created with audit() or modelResiduals().")
  }
}


#' @title Make data frame
#'
#' @description Makes data frame(s) from passed models
#'
#' @param object Object passed to the function
#' @param ... Other modelAudit objects to be plotted together
#' @param variable Variable
#' @param type Type of check; default is \code{res} which stands for "model residuals".
#' Other possible values: \code{eva} - model evaluation
make_dataframe <- function(object, ..., variable, type = "res") {

  if (type == "res" &  !"modelResiduals"  %in% class(object)) object <- modelResiduals(object, variable)
  if (type == "scal" & !"modelResiduals"  %in% class(object)) object <- make_scale_loc_df(modelResiduals(object, variable))
  if (type == "eva" &  !"modelEvaluation" %in% class(object)) object <- attributes(modelEvaluation(object))$CGains

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if (type == "res") {
        if ("modelAudit" %in% class(resp)) resp <- modelResiduals(resp, variable)
        if ("modelResiduals" %in% class(resp)) object <- rbind(object, resp)
      }
      if (type == "scal") {
        if ("modelAudit" %in% class(resp)) resp <- modelResiduals(resp, variable)
        object <- rbind(object, make_scale_loc_df(resp))
      }
      if (type == "eva") {
        if ("modelAudit" %in% class(resp)) resp <- modelEvaluation(resp)
        if ("modelEvaluation" %in% class(resp)) object <- rbind(object, attributes(resp)$CGains)
      }
    }
  }
  return(object)
}


#' @title Extra variables for scaleLocation plot
#'
#' @description Function to generate extra variables for scaleLocation plot
#'
#' @param object An audited model
make_scale_loc_df <- function(object) {browser()
  resultDF <- data.frame(std.residuals = object$std.res, values = object$val)
  resultDF$sqrt.std.residuals <- sqrt(abs(resultDF$std.residuals))
  resultDF$label <- object$label[1]
  resultDF$peak <- (abs(object$std.res) >= cummax(abs(object$std.res)))
  return(resultDF)
}



#' @title DrWhy's wrapper for geom_point function
#'
#' @description Function which draws point layers in desired order
#'
#' @param df Data frame prepared by (\code{make_dataframe}) function
#' @param smooth Logical, if set to \code{TRUE} point are drawn with alpha (set in \code{alpha_val}
#' argument).  Default is \code{FALSE}
#' @param alpha_val Numeric, level of alpha of points when smooth is drawn
drwhy_geom_point <- function(df, smooth = FALSE, alpha_val) {
  # ordering data to get rigth order of points on the plot
  df <- df[order(-as.numeric(factor(df$label))), ]

  geom_point(data = df,
             aes_string(colour = "label"),
             alpha = ifelse(smooth == TRUE, alpha_val, 1),
             stroke = 0)
}

#' @title DrWhy's wrapper for geom_smooth function
#'
#' @description Function which draws smooth layers in desired order
#'
#' @param df Data frame prepared by (\code{make_dataframe}) function
drwhy_geom_smooth <- function(df) {
  df$ord <- paste(rev(as.numeric(df$label)), df$label)

  geom_smooth(data = df,
              aes_string(group = "ord", colour = "label"),
              stat = "smooth",
              method = "gam",
              formula = y ~ s(x, bs = "cs"),
              se = FALSE,
              size = 1,
              show.legend = TRUE)
}
