
#' @title Check object
#'
#' @description Checks if the object is of desired class
#'
#' @param object Object passed to the function
#' @param type Type of check; default is \code{res} which stands for "model residuals".
#' @noRd
check_object <- function(object, type = "res") {
  model_type <- switch(type,
                       "res" = "auditor_model_residual",
                       "eva" = "auditor_model_evaluation",
                       "infl" = "auditor_model_cooksdistance",
                       "fit" = "auditor_model_halfnormal",
                       "prfm" = "auditor_model_performance",
                       "exp" = "explainer")
  function_name <- switch(type,
                       "res" = "model_residual()",
                       "eva" = "model_evaluation()",
                       "infl" = "model_cooksdistance()",
                       "fit" = "model_halfnormal()",
                       "prfm" = "model_performance()",
                       "exp" = "explain() from the DALEX package")

  if (!(model_type %in% class(object))) {
    stop(paste0("The function requires an object created with function ", function_name,
                ". Please, see the current workflow in the paper https://arxiv.org/abs/1809.07763"))
  }
}


#' @title Make data frame
#'
#' @description Makes data frame(s) from passed models
#'
#' @param object Object passed to the function
#' @param ... Other model_audit objects to be plotted together
#' @param variable Variable
#' @param type Type of check; default is \code{res} which stands for "model residuals".
#' @param nlabel Number of labels in calculating `model_cooksdistance`
#' @param quant if TRUE values on axis are on quantile scale in `plotHalfNormal`
#' @param values for `plotModelCorrelation`
#' @param scale_error A logical value indicating whether ECDF should be scaled by proportions of positive and negative proportions; `plotECDF`
#' @param outliers Number of outliers to be marked on `plotECDF`
#' @param residuals A logical value indicating whether residuals should be marked on `plotECDF`
#' @param reverse_y A logical value indicating whether values on y axis should be reversed on `plotECDF`
#' @param score Vector of standard scores for modelRankingPlot
#' @param new.score Function for custom score for modelRankingPlot
#' @noRd
make_dataframe <- function(object, ..., variable = NULL, nlabel = NULL, type = "res",
                           quant = NULL, values = NULL, scale_error = TRUE, outliers = NA,
                           residuals = TRUE, reverse_y = FALSE, score = NULL, new.score = NULL) {
  object <- prepare_object(object, variable, nlabel, type, quant, values, scale_error, outliers,
                           reverse_y, score, new.score)
  if (length(list(...)) > 0) {
    for (resp in list(...)) {
      resp <- prepare_object(object = resp, variable, nlabel, type, quant, values, scale_error, outliers,
                             reverse_y, score, new.score)
      if (type %in% c("pca", "corr")) {
        object <- cbind(object, resp)
        object <- subset(object, select = which(!duplicated(names(object))))
      } else {
        object <- rbind(object, resp)
      }
    }
  }
  if (type == "prfm") object <- scaleModelRankingDF(object)
  return(object)
}



#' @title Prepare object for `make_dataframe`` function
#'
#' @param object An audited model
#' @param variable Variable
#' @param nlabel Number of labels
#' @param quant Logical
#' @param values Values
#' @param scale_error Error scaled
#' @param outliers Outliers
#' @param reverse_y y reversed
#' @param score Scores
#' @param new.score New scores
#' @param type Type of model passed
#' @noRd
prepare_object <- function(object, variable, nlabel, type, quant, values, scale_error, outliers, reverse_y,
                           score, new.score) {

  # check if variable is in data frame
  if (!is.null(variable)) {
    if (!variable %in% colnames(object) && variable != "") {
      stop("The model_residual() function requires `variable = '_y_'`,  `variable = _y_hat_`, `variable = NULL`,  or the name of variable from model data frame.")
    }
  }

  #sorting
  if(type  %in% c("res", "scal")){
    if (!is.null(variable)){
      object <- object[order(object[,variable]),]
      object$`_val_` <- object[,variable]
      object$`_variable_` <- variable
    } else {
      object$`_val_` <- 1:nrow(object)
      object$`_variable_` <- "Observations"
    }
  }


  switch(type,
         "rec"  = { object <- make_rec_df(object) },
         "rroc" = { object <- make_rroc_df(object) },
         "scal" = { object <- make_scale_loc_df(object) },
         "dens" = { object <- get_division(object, variable) },
         "pca"  = { object <- make_pca_df(object) },
         "corr" = { object <- make_corr_df(object, values) },
         "ecdf" = { object <- get_tsecdf_df(object, scale_error, outliers, reverse_y) },
         "infl" = { object <- obs_influence_add(object, nlabel) },
         "prfm" = { object <- as.data.frame(object) })
  return(object)
}


make_scale_loc_df <- function(object) {
  result_df <- object
  result_df$`_sqrt_std_residuals_` <- sqrt(abs(result_df$`_std_residuals_`))
  result_df$`_peak_` <- (abs(object$`_std_residuals_`) >= cummax(abs(object$`_std_residuals_`)))
  return(result_df)
}


make_rec_df <- function(object) {
  err <- sort(abs(object$`_residuals_`))
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

  df <- data.frame(rec_x = rec_x, rec_y = rec_y, label = object$`_label_`[1])
  colnames(df) <- paste0("_", colnames(df), sep = "_")
  df
}


make_rroc_df <- function(object) {
  err <- sort(object$`_y_hat_` - object$`_y_`)
  n <- length(err)
  rroc_x <- numeric(n + 1)
  rroc_y <- numeric(n + 1)
  rroc_x[1] <- 0
  rroc_y[1] <- -Inf

  for (i in 1:n) {
    s <- -err[i]
    tErr <- err + s
    rroc_x[i+1] <- sum(tErr[which(tErr > 0)], na.rm = TRUE )
    rroc_y[i+1] <- sum(tErr[which(tErr < 0)], na.rm = TRUE )
  }

  rroc_x <- rroc_x[-1]
  rroc_y <- rroc_y[-1]

  df <- data.frame(rroc_x = rroc_x, rroc_y = rroc_y, label = object$`_label_`[1], curve = TRUE)

  # calculation of the shift equals 0 which is represented on the plot by a dot
  err <- sort(object$`_y_hat_` - object$`_y_`)
  df <- rbind(df, data.frame(rroc_x = sum(err[which(err > 0)], na.rm = TRUE),
                             rroc_y = sum(err[which(err < 0)], na.rm = TRUE),
                             label = object$`_label_`[1],
                             curve = FALSE))
  colnames(df) <- paste0("_", colnames(df), sep = "_")
  df
}


obs_influence_add <- function(object, nlabel) {

  object$`_big_` <- c(rep(TRUE, nlabel), rep(FALSE, nrow(object) - nlabel))

  return(object)
}


get_division <- function(modelData, variable) {
  df <- modelData

  if (is.null(variable)) {
    variable <- "observation index"
    modelData$`_val_`<- 1:nrow(modelData)
  } else if (variable == "") {
    variable <- "observation index"
    modelData$`_val_`<- 1:nrow(modelData)
  } else if (variable == "_y_") {
    modelData$`_val_`<- modelData[, variable]
    variable <- "target variable"
  } else if (variable == "_y_hat_") {
    modelData$`_val_`<- modelData[, variable]
    variable <- "actual response"
  } else {
    modelData$`_val_`<- modelData[, variable]
  }

  if (any(class(modelData$`_val_`) %in% c("numeric", "integer"))) {
    varMedian <- median(modelData$`_val_`)
    df$`_div_` <- ifelse(modelData$`_val_` > varMedian, paste(">", variable, "median"), paste("<=", variable, "median"))
  } else {
    df$`_div_` <- unlist(modelData$`_val_`, use.names = FALSE)
  }

  df$`_div_` <- factor(df$`_div_`)

  rownames(df) <- NULL
  return(df)
}


make_pca_df <- function(object) {
  df <- data.frame(y = object$`_residuals_`)
  colnames(df) <- as.character(object$`_label_`[1])
  object <- df
}


make_corr_df <- function(object, values) {
  '_y_' <- '_y_hat_' <- NULL

  if (values == "fit") {
    df <- subset(object, select = c(`_y_`, `_y_hat_`))
    names(df)[names(df) == "_y_hat_"] <- as.character(object$`_label_`[1])
  } else if (values == "res") {
    df <- data.frame(y = object$`_residuals_`)
    colnames(df)[1] <- as.character(object$`_label_`[1])
  } else {
    stop("Parameter 'values' should take 'fit' or 'res' values.")
  }
  return(df)
}


get_tsecdf_df <- function(object, scale_error, outliers, reverse_y) {
  res <- object$`_residuals_`
  resids <- data.frame(no.obs = 1:(length(res)), res = res, sign = ifelse(res >= 0, "pos", "neg"))
  df <- resids

  dfLower  <- df[which(df$sign == "neg"), ]
  dfHigher <- df[which(df$sign == "pos"), ]
  dfLower$ecd  <- ecdf(dfLower$res)(dfLower$res)
  dfHigher$ecd <- ecdf(dfHigher$res)(dfHigher$res)
  df <- rbind(dfLower, dfHigher)
  df$`_label_` <- object$`_label_`

  if (reverse_y == FALSE) {
    df$ecd <- ifelse(df$sign == "neg", 1 - df$ecd, df$ecd)
  } else {
    df$ecd <- ifelse(df$sign == "neg", df$ecd, 1 - df$ecd)
  }
  # df$ecd <- ifelse(df$sign == "neg", df$ecd, 1 - df$ecd)
  # if (reverse_y == FALSE) df$ecd <- ifelse(df$sign == "neg", 1 - df$ecd, df$ecd)

  if (scale_error == TRUE) {
    negProportion <- sum(df$sign == "neg") / (sum(df$sign == "neg") + sum(df$sign == "pos"))
    posProportion <- 1 - negProportion
    df$ecd <- ifelse(df$sign == "neg", df$ecd * negProportion, df$ecd * posProportion)
  }

  df$big <- FALSE
  if (!is.na(outliers)) {
    df <- df[order(df$res), ]
    df$big <- c(rep(TRUE, outliers), rep(FALSE, nrow(df) - 2 * outliers), rep(TRUE, outliers))
  }

  df$label <- object$label
  return(df)
}


scaleModelRankingDF <- function(df) {
  df_new <- data.frame()
  scores <- unique(df[,"_name_"])
  for(i in scores){
    scoresDF <- df[which(df[,"_name_"] == i),]
    if (!(i %in% c("auc"))) {
      scoresDF[,"_score_"] <- as.numeric(scoresDF[,"_score_"])
      minScore <- min(scoresDF[,"_score_"])
      scoresDF[,"_score_"] <- 1 / scoresDF[,"_score_"]
      scoresDF[,"_score_"] <- scoresDF[,"_score_"] * minScore
    }
    df_new <- rbind(df_new, scoresDF)
  }
  names(df)[names(df) == "_score_"] <- "_value_"
  df_new <- merge(df_new, df, by = c("_label_", "_name_"))

  # preparation of data for the table
  df_new <- df_new[order(df_new[,"_name_"], df_new[,"_label_"]), ]
  df_new$scaled <- unlist(by(df_new[,"_score_"], df_new[,"_name_"], function(x) { x[1] / x }))
  df_new$scaled <- format(as.numeric(df_new$scaled), scientific = FALSE, digits = 3)
  df_new[,"_name_"] <- as.character(df_new[,"_name_"])
  df_new[,"_value_"] <- format(df_new[,"_value_"], scientific = TRUE, digits = 3)

  # set order of scores (levels in factor)
  default_scores <- c("mae", "mse", "rec", "rroc")
  all_scores <- unique(df_new[,"_name_"])
  df_new[,"_name_"] <- factor(paste0("inv\n", df_new[,"_name_"]),
                        levels = paste0("inv\n", c(all_scores)))
  rownames(df_new) <- NULL

  # export
  df_new
}


#' @title DrWhy's wrapper for geom_point function
#'
#' @description Function which draws point layers in desired order
#'
#' @param df Data frame prepared by (\code{make_dataframe}) function
#' @param smooth Logical, if set to \code{TRUE} point are drawn with alpha (set in \code{alpha_val}
#' argument).  Default is \code{FALSE}
#' @param alpha_val Numeric, level of alpha of points when smooth is drawn
#' @noRd
drwhy_geom_point <- function(df, smooth = FALSE, alpha_val) {
  `_label_` <- NULL
  # ordering data to get right order of points on the plot
  df <- df[order(-as.numeric(factor(df$`_label_`))), ]

  geom_point(data = df,
             aes(colour = `_label_`),
             alpha = ifelse(smooth == TRUE, alpha_val, 1),
             show.legend = ifelse(smooth == TRUE, FALSE, TRUE),
             stroke = 0)
}


#' @title DrWhy's wrapper for geom_smooth function
#'
#' @description Function which draws smooth layers in desired order
#'
#' @param df Data frame prepared by (\code{make_dataframe}) function
#' @noRd
drwhy_geom_smooth <- function(df) {
  'ord' <- '_label_' <- NULL

  df$ord <- paste(rev(as.numeric(df$`_label_`)), df$`_label_`)

  geom_smooth(data = df,
              aes(group = ord, colour = `_label_`),
              stat = "smooth",
              method = "gam",
              formula = y ~ s(x, bs = "cs"),
              se = FALSE,
              size = 1,
              show.legend = TRUE)
}


coord_radar <- function(names_n = 2) {

  rename_data <- function(coord, data) {
    names(data)[which(colnames(data) == "y")] <- "r"
    names(data)[which(colnames(data) == "x")] <- "theta"
    data
  }

  theta_rescale <- function(coord, x, scale_details) {
    rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
    rotate(scales::rescale(x, c(0, 2 * pi), scale_details$theta.range))
  }

  r_rescale <- function(coord, x, scale_details) {
    scales::rescale(x, c(0, 0.4), scale_details$r.range)
  }

  render_bg_function <- function(self, scale_details, theme) {
    scale_details <- rename_data(self, scale_details)

    theta <- if (length(scale_details$theta.major) > 0)
      theta_rescale(self, scale_details$theta.major, scale_details)
    thetamin <- if (length(scale_details$theta.minor) > 0)
      theta_rescale(self, scale_details$theta.minor, scale_details)
    thetafine <- seq(0, 2 * pi, length.out = 100)

    rfine <- c(r_rescale(self, scale_details$r.major, scale_details))

    majortheta <- paste("panel.grid.major.", self$theta, sep = "")
    minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
    majorr     <- paste("panel.grid.major.", self$r,     sep = "")

    ggname <- get("ggname", envir = asNamespace("ggplot2"), inherits = FALSE)
    element_render <- get("element_render", envir = asNamespace("ggplot2"), inherits = FALSE)

    ggname("grill", grid::grobTree(
      element_render(theme, "panel.background"),
      if (length(theta) > 0) element_render(
        theme, majortheta, name = "angle",
        x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
        y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
        id.lengths = rep(2, length(theta)),
        default.units = "native"
      ),
      if (length(thetamin) > 0) element_render(
        theme, minortheta, name = "angle",
        x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
        y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
        id.lengths = rep(2, length(thetamin)),
        default.units = "native"
      ),

      element_render(
        theme, majorr, name = "radius",
        x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
        y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
        id.lengths = rep(length(thetafine), length(rfine)),
        default.units = "native"
      )
    ))
  }

  ggproto("CordRadar", CoordPolar, theta = "x", r = "y", start = - pi / names_n,
          direction = 1, is_linear = function() TRUE, render_bg = render_bg_function)
}

prepare_matrix <- function(df) {
  if (dim(df)[2] == 2) {
    layout_matrix <- matrix(c(1, 4, 3, 2), nrow = 2, byrow = TRUE)
  } else {
    vars <- ncol(df)
    layout_matrix <- matrix(0, nrow = vars, ncol = vars)

    # diagonal axis
    diag(layout_matrix) <- 1L:vars

    # lower and upper triangular part of a matrix
    low_tri <- lower.tri(layout_matrix)
    low_ind <- which(low_tri, arr.ind = TRUE)
    layout_matrix[low_ind] <- vars + 1L:sum(low_tri)

    upp_tri <- upper.tri(layout_matrix)
    upp_ind <- which(upp_tri, arr.ind = TRUE)
    upp_ind <- upp_ind[order(upp_ind[, "row"]), ]
    layout_matrix[upp_ind] <- vars + sum(low_tri) + 1L:sum(upp_tri)
  }
  layout_matrix
}

corr_density <- function(args, data) {
  ggplot(data = data, aes_string(x = args[1])) +
    geom_density(colour = "#160e3b") +
    theme_drwhy() +
    theme(axis.text = element_text(size = 8)) +
    xlab(ifelse(args[2], args[1], "")) +
    ylab(ifelse(args[3], args[1], "")) +
    scale_y_continuous(limits = c(0, as.numeric(args[4]) * 1.2),
                       breaks = scales::pretty_breaks(3))
}

corr_points <- function(args, data) {
  ggplot(data = data, aes_string(x = args[1], y = args[2])) +
    geom_point(colour = "#160e3b", alpha = 0.65, stroke = 0.2) +
    theme_drwhy() +
    theme(axis.text = element_text(size = 8)) +
    xlab(ifelse(args[3], args[1], "")) +
    ylab(ifelse(args[4], args[2], ""))
}
