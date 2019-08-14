context("errors")

test_that("data consistency", {
  expect_error( audit(model.lm, data = Prestige, y = c(1,2)))
})
test_that("objects in plots", {
  expect_error(plotACF(model_lm))
  expect_error(plotAutocorrelation(model_lm))
  expect_error(plotCooksDistance(model_lm))
  expect_error(plotHalfNormal(model_lm))
  expect_error(plotLIFT(model_lm))
  expect_error(plotModelCorrelation(model_lm))
  expect_error(plotModelCorrelation(au_glm, au_rf, values = "new"))
  expect_error(plotModelPCA(model_lm))
  expect_error(plotModelRanking(model_lm))
  expect_error(plotPrediction(model_lm))
  expect_error(plotResidual(model_lm))
  expect_error(plotResidualBoxplot(model_lm))
  expect_error(plotResidualDensity(model_lm))
  expect_error(plotResidualDensity(au_lm, variable = "women"))
  expect_error(plotROC(model_lm))
  expect_error(plotRROC(model_lm))
  expect_error(plotScaleLocation(model_lm))
  expect_error(plotTwoSidedECDF(model_lm))
})


test_that("objects in calculation functions", {
  expect_error(modelProfmance(model_lm))
  expect_error(modelResiduals(au_lm, variable = "ok"))
  expect_error(modelResiduals(model_lm))
  expect_error(modelEvaluation(model_lm))
})
