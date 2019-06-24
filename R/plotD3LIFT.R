#' @export
#' @rdname plotD3LIFT
plotD3LIFT <- function(object, ..., scale_plot = FALSE) {

  # some safeguard
  rpp <- tp <- label <- NULL

  xTitle <- "Rate of positive prediction"
  yTitle <- "True positive"
  chartTitle <- "LIFT Chart"

  n <- length(list(object, ...))

  # check if passed object is of class "modelResiduals" or "modelAudit"
  check_object(object, type = "eva")

  modelNames <- lapply(list(object, ...), function(x) x$label)

  # prepare data frame for ideal and dummy model
  ideal_df <- attributes(modelEvaluation(object))$idealCGains
  ideal_df <- rbind(c(0, 0, 0, "ideal"), ideal_df)

  cols <- c("rpp", "tp", "alpha")
  ideal_df[,cols] = apply(ideal_df[,cols], 2, function(x) as.numeric(x))

  random_df <- data.frame(rpp = c(0, 1),
                          tp =  c(0, max(ideal_df$tp)),
                          alpha = c(0, 1),
                          label = c("random", "random"))

  df2 <- rbind(ideal_df, random_df)

  # prepare data frame for the main ggplot object
  df1 <- make_dataframe(object, ..., variable = variable, type = "eva")
  #for (lab in unique(df1$label)) df1 <- rbind(df1, c("0", "0", "0", lab))

  df1[,cols] = apply(df1[,cols], 2, function(x) as.numeric(x))

  # new variable to set different style of line for ideal and dummy models
  df1$line <- "1"
  df2$line <- "2"
  df <- rbind(df2, df1)

  #:#
  ymax <- max(df$tp)
  ymin <- min(df$tp)

  lineData <- split(df, f = df$label)

  temp <- jsonlite::toJSON(list(lineData[c("ideal","random")],
                                lineData[setdiff(names(lineData), c("ideal","random"))]))

  options <- list(ymax = ymax, ymin = ymin,
                  modelNames = modelNames,
                  scalePlot = scale_plot, n = n,
                  xTitle = xTitle, yTitle = yTitle, chartTitle = chartTitle)

  r2d3::r2d3(data = temp, script = system.file("d3js/plotCurve.js", package = "auditor"),
             dependencies = system.file("d3js/colorsDrWhy.js", package = "auditor"),
             css = system.file("d3js/themeDrWhy.css", package = "auditor"),
             d3_version = 4,
             options = options)
}
