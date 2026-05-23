testthat::test_that("shouldShowAdjustmentOutputControls is false without model", {
  testthat::expect_false(shouldShowAdjustmentOutputControls(NULL))
})

testthat::test_that("shouldShowAdjustmentOutputControls is false with no adjustments", {
  model = stats::lm(mpg ~ wt, data = mtcars)
  attr(model, "wmfm_adjustment_variables") = character(0)

  testthat::expect_false(shouldShowAdjustmentOutputControls(model))
})

testthat::test_that("shouldShowAdjustmentOutputControls is true with adjustments", {
  model = stats::lm(mpg ~ wt + hp, data = mtcars)
  attr(model, "wmfm_adjustment_variables") = "hp"

  testthat::expect_true(shouldShowAdjustmentOutputControls(model))
})
