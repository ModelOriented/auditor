context("plots")

source("objects_for_tests.R")

test_that("plotACF", {
  expect_is(plotACF(au.lm, "income"), "gg")
  expect_is(plotACF(au.rf), "gg")
})

test_that("plotAutocorrelation", {
  expect_is(plotAutocorrelation(au.lm, "income"), "gg")
  expect_is(plotAutocorrelation(au.rf, au.lm), "gg")
})

test_that("plotCook", {
  expect_is(plotCooksDistance(au.lm), "gg")
  expect_is(plotCooksDistance(au.rf), "gg")
})

test_that("plotPrediction", {
  expect_is(plotPrediction(au.rf), "gg")
  expect_is(plotPrediction(au.rf, variab), "gg")
  expect_is(plotPrediction(au.rf, smooth = TRUE), "gg")
})

test_that("plotResiduals", {
  expect_is(plotResidual(au.glm), "gg")
  expect_is(plotResidual(au_expl_lm), "gg")
  expect_is(plotResidual(au.rf, "Prewt"), "gg")
})

test_that("plotResidualBoxplot", {
  expect_is(plotResidualBoxplot(au.rf, au.lm), "gg")
})

test_that("plotResidualDeensity", {
  expect_is(plotResidualDensity(au.lm), "gg")
  expect_is(plotResidualDensity(au.lm, variable = "women"), "gg")
  expect_is(plotResidualDensity(au.lm, variable = ""), "gg")
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

test_that("plotLIFT", {
  expect_is(plotLIFT(au.class.glm2), "gg")
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
  expect_is(plot(au.lm, type="Autocorrelation", score = TRUE), "gg")
  expect_is(plot(au.lm, type="CooksDistance", print=FALSE), "gg")
  expect_is(plot(au.lm, au.rf, type="ModelRanking"), "gtable")
  expect_is(plot(au.lm, au.rf, type="TwoSidedECDF"), "gg")
  expect_is(plot(au.glm, au.rf, type="ModelPCA"), "gg")
  expect_is(plot(au.lm, type="ResidualDensity"), "gg")
  expect_is(plot(au.class.glm2,  type="LIFT"), "gg")
  expect_is(plot(au.glm, au.rf, type="ModelCorrelation"), "gg")
  expect_is(plot(au.glm, au.rf, type="ModelCorrelation", values = "Residuals"), "gg")
  expect_is(plot(au.lm, au.rf, type="Prediction"), "gg")
  expect_is(plot(au.lm, type="Residual"), "gg")
  expect_is(plot(au.lm, type="ResidualBoxplot"), "gg")
  expect_is(plot(au.lm, type="ScaleLocation", score = TRUE), "gg")
  expect_is(plot(au.glm, type="HalfNormal", sim=10), "gg")
  expect_is(plot(au.class.glm, au.class.glm2, type="ROC"), "gg")
  expect_is(plot(au.glm, au.rf, type="RROC"), "gg")
  expect_is(plot(au.glm, au.rf, type="REC"), "gg")
  expect_error(plot(au.lm, type="wrongType"))
})


test_that("multiple plots on grid", {
  expect_is(plot(au.lm, au.rf, type=c("Prediction", "Residual"), grid = TRUE), "gtable")
})


test_that("plot, grid equals FALSE", {
  expect_is(plot(au.lm, au.rf, type=c("Prediction", "Residual"), grid = FALSE, ask = FALSE), "auditorPlotList")
})

test_that("plot type is not provided", {
  expect_is(plot(cd.lm), "gg")
  expect_is(plot(mp.lm, table=FALSE), "gg")
  expect_is(plot(mf.lm), "gg")
})


test_that("plot type is not provided", {
new_score <- function(object) sum(sqrt(abs(object$residuals)))
new_score2 <- function(object) sum(sqrt(abs(object$residuals))+1)
lm.mp <- modelPerformance(au.lm,
                          scores = c("MAE", "MSE", "REC", "RROC"),
                          new.score = list(n1=new_score, n2=new_score2))
lm.mp2 <- modelPerformance(au.lm,
                          scores = c("MAE", "MSE", "REC", "RROC"),
                          new.score = new_score)
expect_is(plot(lm.mp, lm.mp2, type="ModelRanking", table=FALSE), "gg")
})
