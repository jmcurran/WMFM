testthat::test_that("equation display metadata keeps adjustment predictors in fitted formula", {
  dat = data.frame(
    y = c(1, 2, 3, 4, 5, 6),
    x = c(2, 3, 4, 5, 6, 7),
    z = c(10, 9, 8, 7, 6, 5)
  )

  model = stats::lm(y ~ x + z, data = dat)
  attr(model, "wmfm_adjustment_variables") = "z"

  roleMetadata = buildEquationDisplayRoleMetadata(model)

  testthat::expect_true(grepl("z", deparse(stats::formula(model)), fixed = TRUE))
  testthat::expect_setequal(roleMetadata$allPredictors, c("x", "z"))
  testthat::expect_identical(roleMetadata$adjustmentPredictors, "z")
  testthat::expect_identical(roleMetadata$primaryPredictors, "x")
  testthat::expect_true(roleMetadata$hasAdjustments)
})

testthat::test_that("equation display summary distinguishes primary and adjustment variables", {
  roleMetadata = list(
    allPredictors = c("studyHours", "attendance", "age"),
    primaryPredictors = c("studyHours", "attendance"),
    adjustmentPredictors = "age",
    hasAdjustments = TRUE
  )

  out = buildEquationDisplayRoleSummary(roleMetadata)

  testthat::expect_length(out, 3)
  testthat::expect_match(out[[1]], "Primary predictors:", fixed = TRUE)
  testthat::expect_match(out[[2]], "Adjustment variables:", fixed = TRUE)
  testthat::expect_match(out[[3]], "Full fitted equation shown below includes both", fixed = TRUE)
})

testthat::test_that("models without adjustment variables keep existing equation behavior metadata", {
  dat = data.frame(
    y = c(1, 2, 3, 4),
    x = c(1, 2, 3, 4)
  )

  model = stats::lm(y ~ x, data = dat)

  roleMetadata = buildEquationDisplayRoleMetadata(model)
  out = buildEquationDisplayRoleSummary(roleMetadata)

  testthat::expect_identical(roleMetadata$adjustmentPredictors, character(0))
  testthat::expect_identical(roleMetadata$primaryPredictors, "x")
  testthat::expect_false(roleMetadata$hasAdjustments)
  testthat::expect_identical(out, character(0))
})
