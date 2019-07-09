
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
                       "eva" = "modelEvaluation",
                       "infl" = "observationInfluence",
                       "fit" = "modelFit")

  if (!(model_type %in% class(object) || "modelAudit" %in% class(object))) {
    stop(paste0("The function requires an object created with audit() or ", model_type, "()."))
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
#' @param nlabel Number of labels in calculating `observationInfluence`
#' @param quant if TRUE values on axis are on quantile scale in `plotHalfNormal`
#' Other possible values: \code{eva} - model evaluation
make_dataframe <- function(object, ..., variable = NULL, nlabel = NULL, type = "res", quant = NULL) {

  object <- prepare_object(object, variable, nlabel, type, quant)
  if (length(list(...)) > 0) {
    for (resp in list(...)) {
      resp <- prepare_object(object = resp, variable, nlabel, type, quant)
      if (type == "pca") {
        object <- cbind(object, resp)
      } else {
        object <- rbind(object, resp)
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
make_scale_loc_df <- function(object) {
  resultDF <- data.frame(std.residuals = object$std.res, values = object$val)
  resultDF$sqrt.std.residuals <- sqrt(abs(resultDF$std.residuals))
  resultDF$label <- object$label[1]
  resultDF$peak <- (abs(object$std.res) >= cummax(abs(object$std.res)))
  return(resultDF)
}


make_rec_df <- function(object) {
  err <- sort(abs(object$res))
  err <- c(0, err)
  n <- length(err)
  rec_x <- numeric(n)
  rec_y <- numeric(n)
  rec_x[1] <- rec_y[1] <- correct <- absDev <- 0
  for(i in 2:n) {
    if (err[i] > err[i-1]) absDev <- correct / n
    rec_x[i] <- err[i]
    rec_y[i] <- absDev
    correct <- correct + 1
  }

  df <- data.frame(rec_x = rec_x, rec_y = rec_y, label = object$label[1])
  return(df)
}


make_rroc_df <- function(object) {
  err <- sort(object$fitted.values - object$y)
  n <- length(err)
  rroc_x <- numeric(n + 2)
  rroc_y <- numeric(n + 2)
  rroc_x[1] <- 0
  rroc_y[1] <- -Inf

  for (i in 1:n) {
    s <- -err[i]
    tErr <- err + s
    rroc_x[i+1] <- sum(tErr[which(tErr > 0)], na.rm = TRUE )
    rroc_y[i+1] <- sum(tErr[which(tErr < 0)], na.rm = TRUE )
  }

  rroc_x[n + 2] <- Inf
  rroc_y[n + 2] <- 0

  df <- data.frame(rroc_x = rroc_x, rroc_y = rroc_y, label = object$label[1], curve = TRUE)

  # calculation of the shift equals 0 which is represented on the plot by a dot
  err <- sort(object$fitted.values - object$y)
  df <- rbind(df, data.frame(rroc_x = sum(err[which(err > 0)], na.rm = TRUE),
                             rroc_y = sum(err[which(err < 0)], na.rm = TRUE),
                             label = object$label[1],
                             curve = FALSE))
  return(df)
}


obs_influence_add <- function(object, nlabel) {

  df <- observationInfluence(object)
  df$big <- c(rep(TRUE, nlabel), rep(FALSE, nrow(df) - nlabel))
  return(df)
}




get_division <- function(modelData) {
  variable <- modelData$variable[1]
  df <- modelData
  if (class(modelData$val) %in% c("numeric", "integer")) {
    varMedian <- median(modelData$val)
    df$div <- ifelse(modelData$val > varMedian, paste(">", variable, "median"), paste("<=", variable, "median"))
  } else {
    df$div <- modelData$val
  }
  return(df)
}

make_pca_df <- function(object) {
  df <- data.frame(y = object$res)
  colnames(df) <- as.character(object$label[1])
  object <- df
}


#' @title Prepare object for `make_dataframe`` function
#'
#' @param object An audited model
#' @param variable Variable
#' @param nlabel Number of labels
#' @param quant Logical
#' @param type Type of model passed
prepare_object <- function(object, variable, nlabel, type, quant) {
  if ("modelAudit" %in% class(object)) {
    if (type %in% c("res", "rec", "rroc", "scal", "dens", "pca")) object <- modelResiduals(object, variable)
    switch(type,
           "eva"  = { object <- modelEvaluation(object) },
           "infl" = { object <- obs_influence_add(object, nlabel) },
           "fit"  = { object <- modelFit(object, quant.scale = quant) })
  }
  switch(type,
         "rec"  = { object <- make_rec_df(object) },
         "rroc" = { object <- make_rroc_df(object) },
         "scal" = { object <- make_scale_loc_df(object) },
         "dens" = { object <- get_division(object) },
         "eva"  = { object <- object },
         "pca"  = { object <- make_pca_df(object) })
  return(object)
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
