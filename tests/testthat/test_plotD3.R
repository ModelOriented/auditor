context("plotD3")

source("objects_for_tests.R")

test_that("plotD3Residual", {
  expect_is(plotD3Residual(au.glm, smooth = TRUE, point_count = 5), "r2d3")
  expect_is(plotD3Residual(au_expl_lm, std_residuals = TRUE), "r2d3")
  expect_is(plotD3Residual(au.rf, variable = "Prewt"), "r2d3")
  expect_is(plotD3Residual(au.rf, au.lm, single_plot = FALSE, scale_plot = TRUE), "r2d3")
  expect_error(plotD3Residual(au.lm, points = FALSE, smooth = FALSE))
  expect_error(plotD3Residual(glm_mr, rf_mr))
  expect_error(plotD3Residual(list(1,2,3)))
})

test_that("plotD3Autocorrelation", {
  expect_is(plotD3Autocorrelation(au.rf, au.lm, smooth = TRUE, point_count = 5), "r2d3")
  expect_is(plotD3Autocorrelation(au.lm, variable = "income"), "r2d3")
  expect_is(plotD3Autocorrelation(au.rf, au.lm, single_plot = FALSE, scale_plot = TRUE), "r2d3")
  expect_is(plotD3Autocorrelation(au.rf, au.lm, smooth = TRUE, point_count = 5), "r2d3")
  expect_error(plotD3Autocorrelation(au.lm, points = FALSE, smooth = FALSE))
  expect_error(plotD3Autocorrelation(glm_mr, rf_mr))
  expect_error(plotD3Autocorrelation(list(1,2,3)))
})

test_that("plotD3Prediction", {
  expect_is(plotD3Prediction(au.glm, smooth = TRUE, point_count = 5), "r2d3")
  expect_is(plotD3Prediction(au_expl_lm), "r2d3")
  expect_is(plotD3Prediction(au.rf, variable = "Prewt"), "r2d3")
  expect_is(plotD3Prediction(au.rf, au.lm, single_plot = FALSE, scale_plot = TRUE), "r2d3")
  expect_error(plotD3Prediction(au.lm, points = FALSE, smooth = FALSE))
  expect_error(plotD3Prediction(glm_mr, rf_mr))
  expect_error(plotD3Prediction(list(1,2,3)))
})

test_that("plotD3ScaleLocation", {
  expect_is(plotD3ScaleLocation(au.glm, smooth = TRUE, point_count = 5), "r2d3")
  expect_is(plotD3ScaleLocation(au_expl_lm, peaks = TRUE), "r2d3")
  expect_is(plotD3ScaleLocation(au.rf, variable = "Prewt"), "r2d3")
  expect_is(plotD3ScaleLocation(au.rf, au.lm, single_plot = FALSE, scale_plot = TRUE), "r2d3")
  expect_error(plotD3ScaleLocation(au.lm, points = FALSE, smooth = FALSE))
  expect_error(plotD3ScaleLocation(glm_mr, rf_mr))
  expect_error(plotD3ScaleLocation(list(1,2,3)))
})

test_that("plotD3CooksDistance", {
  expect_is(plotD3CooksDistance(au.glm, cd.lm), "r2d3")
  expect_is(plotD3CooksDistance(au.rf, au.lm, single_plot = FALSE), "r2d3")
  expect_is(plotD3CooksDistance(au.rf, au.lm, single_plot = TRUE, scale_plot = TRUE), "r2d3")
  expect_error(plotD3CooksDistance(list(1,2,3)))
})

test_that("plotD3LIFT", {
  expect_is(plotD3LIFT(au.class.glm), "r2d3")
  expect_is(plotD3LIFT(au.class.glm, au.class.glm2, scale_plot = TRUE), "r2d3")
})

test_that("plotD3REC", {
  expect_is(plotD3REC(au.glm), "r2d3")
  expect_is(plotD3REC(au.glm, au.lm, scale_plot = TRUE), "r2d3")
})

test_that("plotD3ACF", {
  expect_is(plotD3ACF(au.glm), "r2d3")
  expect_is(plotD3ACF(au.glm, au.lm, scale_plot = TRUE), "r2d3")
  expect_is(plotD3ACF(au.glm, au.lm, scale_plot = TRUE, alpha = 0.1), "r2d3")
})

test_that("plotD3HalfNormal", {
  expect_is(plotD3HalfNormal(au.glm), "r2d3")
  expect_is(plotD3HalfNormal(au.glm, au.lm, scale_plot = TRUE), "r2d3")
})
