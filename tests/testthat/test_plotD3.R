context("plotD3")

source("objects_for_tests.R")

test_that("plotD3Residual", {
  expect_is(plotD3Residual(au.glm, smooth = TRUE, point_count = 5), "r2d3")
  expect_is(plotD3Residual(au_expl_lm, std_residuals = TRUE), "r2d3")
  expect_is(plotD3Residual(au.rf, variable = "Prewt"), "r2d3")
  expect_is(plotD3Residual(au.rf, au.lm, single_plot = FALSE, scale_plot = TRUE), "r2d3")
  expect_error(plotD3Residual(au.lm, points = FALSE, smooth = FALSE))
  expect_error(plotD3Residual(glm_mr, rf_mr))
})

test_that("plotD3Autocorrelation", {
  expect_is(plotD3Autocorrelation(au.rf, au.lm, smooth = TRUE, point_count = 5), "r2d3")
  expect_is(plotD3Autocorrelation(au.lm, variable = "income", score = TRUE), "r2d3")
  expect_is(plotD3Autocorrelation(au.rf, au.lm, single_plot = FALSE, scale_plot = TRUE), "r2d3")
  expect_is(plotD3Autocorrelation(au.rf, au.lm, smooth = TRUE, point_count = 5), "r2d3")
  expect_error(plotD3Autocorrelation(au.lm, points = FALSE, smooth = FALSE))
  expect_error(plotD3Autocorrelation(glm_mr, rf_mr))
})

test_that("plotD3Prediction", {
  expect_is(plotD3Prediction(au.glm, smooth = TRUE, point_count = 5), "r2d3")
  expect_is(plotD3Prediction(au_expl_lm), "r2d3")
  expect_is(plotD3Prediction(au.rf, variable = "Prewt"), "r2d3")
  expect_is(plotD3Prediction(au.rf, au.lm, single_plot = FALSE, scale_plot = TRUE), "r2d3")
  expect_error(plotD3Prediction(au.lm, points = FALSE, smooth = FALSE))
  expect_error(plotD3Prediction(glm_mr, rf_mr))
})
