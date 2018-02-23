source("objects_for_tests.R")

test_that("plotACF", {
  expect_is(plotACF(au.lm, "income"), "gg")
  expect_is(plotACF(au.rf), "gg")
})

test_that("plotAutocorrelation", {
  expect_is(plotAutocorrelation(au.lm, "income"), "gg")
  expect_is(plotAutocorrelation(au.rf), "gg")
})

test_that("plotCook", {
  expect_is(plotCook(au.lm), "gg")
  expect_is(plotCook(au.rf), "gg")
})

test_that("plotResiduals", {
  expect_is(plotResiduals(au.glm), "gg")
  expect_is(plotResiduals(au.rf, "Prewt"), "gg")
})

test_that("plotScaleLocation", {
  expect_is(plotScaleLocation(au.glm), "gg")
  expect_is(plotScaleLocation(au.rf, "Prewt"), "gg")
})

test_that("plotHalfNormal", {
  expect_is(plotHalfNormal(au.glm, sim=10), "gg")
  expect_is(plotHalfNormal(au.lm, sim=10, quant.scale = TRUE), "gg")
  expect_is(plotHalfNormal(iris.rf, sim=10), "gg")
})


test_that("plot", {
  expect_is(plot(au.lm, type="ACF"), "gg")
  expect_is(plot(au.lm, type="Autocorrelation"), "gg")
  expect_is(plot(au.lm, type="Cook", print=FALSE), "gg")
  expect_is(plot(au.lm, type="Residuals"), "gg")
  expect_is(plot(au.lm, type="ScaleLocation"), "gg")
  expect_is(plot(au.glm, type="HalfNormal", sim=10), "gg")
  expect_error(plot(au.lm, type="wrongType"))
})
