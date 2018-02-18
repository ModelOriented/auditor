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

test_that("plotScaleLocation", {
  expect_is(plotHalfNormal(au.glm, sim=10), "gg")
  expect_is(plotHalfNormal(au.lm, sim=10, quant.scale = TRUE), "gg")
})
