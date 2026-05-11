testthat::test_that("buildFittedEquationContentUi includes role summary for data frame equation output", {
  roleSummary = c("Primary predictors: x", "Adjustment variables: z")
  roleSummaryUi = buildFittedEquationRoleSummaryUi(roleSummary)

  eq = data.frame(
    condition = "Default",
    equation = "y = 1.00 + 2.00 * x + 3.00 * z",
    stringsAsFactors = FALSE
  )

  out = buildFittedEquationContentUi(eq, roleSummaryUi = roleSummaryUi)
  outText = as.character(out)

  testthat::expect_match(outText, "Predictor roles in this fitted model", fixed = TRUE)
  testthat::expect_match(outText, "Primary predictors: x", fixed = TRUE)
  testthat::expect_match(outText, "Adjustment variables: z", fixed = TRUE)
  testthat::expect_match(outText, "y = 1.00 + 2.00 \* x \+ 3.00 \* z")
})

testthat::test_that("buildFittedEquationContentUi includes role summary for character equation output", {
  roleSummary = c("Primary predictors: x", "Adjustment variables: z")
  roleSummaryUi = buildFittedEquationRoleSummaryUi(roleSummary)

  eq = "y = 1.00 + 2.00 * x + 3.00 * z"
  out = buildFittedEquationContentUi(eq, roleSummaryUi = roleSummaryUi)
  outText = as.character(out)

  testthat::expect_match(outText, "Predictor roles in this fitted model", fixed = TRUE)
  testthat::expect_match(outText, "Primary predictors: x", fixed = TRUE)
  testthat::expect_match(outText, "Adjustment variables: z", fixed = TRUE)
  testthat::expect_match(outText, "y = 1.00 + 2.00 \* x \+ 3.00 \* z")
})
