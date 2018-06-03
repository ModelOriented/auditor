context("plots")

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
  expect_is(plotCooksDistance(au.lm), "gg")
  expect_is(plotCooksDistance(au.rf), "gg")
})

test_that("plotResiduals", {
  expect_is(plotResidual(au.glm), "gg")
  expect_is(plotResidual(au_expl_lm), "gg")
  expect_is(plotResidual(au.rf, "Prewt"), "gg")
})

test_that("plotScaleLocation", {
  expect_is(plotScaleLocation(au.glm), "gg")
  expect_is(plotScaleLocation(au.rf, "Prewt"), "gg")
})

test_that("plotHalfNormal", {
  expect_is(plotHalfNormal(au.glm, sim=10), "gg")
  expect_is(plotHalfNormal(au.lm, sim=10, quant.scale = TRUE), "gg")
  expect_is(plotHalfNormal(au.class.rf, sim=10), "gg")
})

test_that("plotROC", {
  expect_is(plotROC(au.class.glm), "gg")
  expect_is(plotROC(au.class.glm, au.class.glm2), "gg")
})

test_that("plotRROC", {
  expect_is(plotRROC(au.glm), "gg")
  expect_is(plotRROC(au.glm, au.rf), "gg")
})

test_that("plotREC", {
  expect_is(plotREC(au.glm), "gg")
  expect_is(plotREC(au.glm, au.rf), "gg")
})

test_that("plot", {
  expect_is(plot(au.lm, type="ACF"), "gg")
  expect_is(plot(au.lm, type="Autocorrelation"), "gg")
  expect_is(plot(au.lm, type="CooksDistance", print=FALSE), "gg")
  expect_is(plot(au.lm, au.rf, type="ModelRanking"), "gg")
  expect_is(plot(au.lm, au.rf, type="TwoSidedECDF"), "gg")
  expect_is(plot(au.glm, au.rf, type="ModelPCA"), "gg")
  expect_is(plot(au.lm, type="ResidualDensity"), "gg")
  expect_is(plot(au.glm, au.rf, type="ModelCorrelation"), "gg")
  expect_is(plot(au.lm, au.rf, type="Prediction"), "gg")
  expect_is(plot(au.lm, type="Residual"), "gg")
  expect_is(plot(au.lm, type="ScaleLocation"), "gg")
  expect_is(plot(au.glm, type="HalfNormal", sim=10), "gg")
  expect_is(plot(au.class.glm, au.class.glm2, type="ROC"), "gg")
  expect_is(plot(au.glm, au.rf, type="RROC"), "gg")
  expect_is(plot(au.glm, au.rf, type="REC"), "gg")
  expect_error(plot(au.lm, type="wrongType"))
})
