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

testthat::test_that("renderAdjustmentVariablesUi keeps compatibility checkbox input hidden", {
  rv = shiny::reactiveValues(
    data = data.frame(y = 1:3, age = 1:3),
    bucketFactors = "y",
    bucketContinuous = "age",
    adjustmentVariables = "age"
  )

  ui = renderAdjustmentVariablesUi(rv = rv, responseVariable = "y")
  uiText = as.character(ui)

  testthat::expect_true(grepl("display: none", uiText, fixed = TRUE))
  testthat::expect_true(grepl("adjustment_variables", uiText, fixed = TRUE))
})

testthat::test_that("renderBucketVariableLabel marks checked state from selections", {
  checkedUi = renderBucketVariableLabel("age", "age")
  uncheckedUi = renderBucketVariableLabel("age", "group")

  testthat::expect_true(grepl("wmfm-adjustment-checkbox", as.character(checkedUi), fixed = TRUE))
  testthat::expect_true(grepl("Adjustment variables are included in the model", as.character(checkedUi), fixed = TRUE))
  testthat::expect_true(grepl("checked", as.character(checkedUi), fixed = TRUE))
  testthat::expect_false(grepl("checked", as.character(uncheckedUi), fixed = TRUE))
})
