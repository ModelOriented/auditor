context("errors")

test_that("data consistency", {
  expect_error( audit(model.lm, data = Prestige, y = c(1,2)))
})
test_that("objects in plots", {
  expect_error(plotACF(model.lm))
  expect_error(plotAutocorrelation(model.lm))
  expect_error(plotCooksDistance(model.lm))
  expect_error(plotHalfNormal(model.lm))
  expect_error(plotLIFT(model.lm))
  expect_error(plotModelCorrelation(model.lm))
  expect_error(plotModelPCA(model.lm))
  expect_error(plotModelRanking(model.lm))
  expect_error(plotPrediction(model.lm))
  expect_error(plotResidual(model.lm))
  expect_error(plotResidualBoxplot(model.lm))
  expect_error(plotResidualDensity(model.lm))
  expect_error(plotResidualDensity(au.lm, variable = "women"))
  expect_error(plotROC(model.lm))
  expect_error(plotRROC(model.lm))
  expect_error(plotScaleLocation(model.lm))
  expect_error(plotTwoSidedECDF(model.lm))
})


test_that("objects in calculation functions", {
  expect_error(modelProfmance(model.lm))
  expect_error(modelResiduals(au.lm, variable = "ok"))
  expect_error(modelResiduals(model.lm))
  expect_error(modelEvaluation(model.lm))
})
