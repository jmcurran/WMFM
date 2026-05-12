testthat::test_that("buildEligibleAdjustmentVariables includes factor and numeric predictors only", {
  out = buildEligibleAdjustmentVariables(
    responseVariable = "y",
    factorVariables = c("group", "y"),
    continuousVariables = c("age", "score")
  )

  testthat::expect_identical(out, c("group", "age", "score"))
})

testthat::test_that("sanitizeAdjustmentVariables stores checked eligible variables", {
  out = sanitizeAdjustmentVariables(
    selectedVariables = c("age", "age", "", "group", "notInPredictors"),
    eligibleVariables = c("group", "age", "score")
  )

  testthat::expect_identical(out, c("age", "group"))
})

testthat::test_that("adjustment variable selection does not remove variables from formula", {
  formulaText = "y ~ age + group"
  adjustmentVariables = sanitizeAdjustmentVariables(
    selectedVariables = "age",
    eligibleVariables = c("group", "age")
  )

  testthat::expect_identical(adjustmentVariables, "age")
  testthat::expect_true(grepl("age", formulaText, fixed = TRUE))
  testthat::expect_true(grepl("group", formulaText, fixed = TRUE))
})


testthat::test_that("buildAdjustmentMetadata drops stale variables not in fitted predictors", {
  out = buildAdjustmentMetadata(
    selectedVariables = c("age", "group", ""),
    formulaPredictors = c("group", "score")
  )

  testthat::expect_identical(out, "group")
})
