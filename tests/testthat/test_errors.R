context("errors")

test_that("data consistency", {
  expect_error( audit(model.lm, data = carData::Prestige, y = c(1,2)))
})
