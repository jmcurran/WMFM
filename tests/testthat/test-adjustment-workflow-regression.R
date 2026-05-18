testthat::test_that("adjustment workflow contract is preserved end to end", {
  rawData = data.frame(
    outcome = c(10, 11, 13, 15, 16, 18, 19, 21),
    studyHours = c(1, 2, 2, 3, 3, 4, 4, 5),
    attendance = c(70, 75, 78, 80, 82, 85, 87, 90),
    age = c(18, 19, 20, 19, 21, 22, 20, 23)
  )

  eligibleAdjustments = buildEligibleAdjustmentVariables(
    responseVariable = "outcome",
    factorVariables = character(0),
    continuousVariables = c("studyHours", "attendance", "age")
  )

  selectedAdjustments = sanitizeAdjustmentVariables(
    selectedVariables = c("age", "age", ""),
    eligibleVariables = eligibleAdjustments
  )

  testthat::expect_identical(selectedAdjustments, "age")

  fittedModel = stats::lm(outcome ~ studyHours + attendance + age, data = rawData)
  attr(fittedModel, "wmfm_adjustment_variables") = selectedAdjustments

  testthat::expect_true(grepl("age", deparse(stats::formula(fittedModel)), fixed = TRUE))

  roleMetadata = buildEquationDisplayRoleMetadata(fittedModel)
  roleSummary = buildEquationDisplayRoleSummary(roleMetadata)

  testthat::expect_identical(roleMetadata$adjustmentPredictors, "age")
  testthat::expect_setequal(roleMetadata$primaryPredictors, c("studyHours", "attendance"))
  testthat::expect_match(roleSummary[[1]], "Primary predictors: studyHours, attendance", fixed = TRUE)
  testthat::expect_match(roleSummary[[2]], "Adjustment variables: age", fixed = TRUE)

  deterministicPrompt = suppressWarnings(lmToExplanationPrompt(fittedModel))
  adjustmentBlock = buildAdjustmentVariablePromptBlock(fittedModel)
  termEvidenceBlock = buildLmTermEvidencePromptBlock(fittedModel)

  testthat::expect_match(deterministicPrompt, "Adjustment-variable interpretation guidance:", fixed = TRUE)
  testthat::expect_match(adjustmentBlock, "Do not interpret adjustment-variable coefficients as substantive findings.", fixed = TRUE)
  testthat::expect_match(termEvidenceBlock, "age: .*adjustment variable", perl = TRUE)
  testthat::expect_match(termEvidenceBlock, "mention age only as adjusted-for variables", fixed = TRUE)
  testthat::expect_no_match(termEvidenceBlock, "For weak additive terms \\(age\\)", perl = TRUE)
})

testthat::test_that("adjustment workflow behavior is unchanged when no adjustments are selected", {
  rawData = data.frame(
    outcome = c(5, 7, 8, 9, 11, 12),
    x = c(1, 2, 3, 4, 5, 6),
    z = c(2, 2, 3, 3, 4, 4)
  )

  eligibleAdjustments = buildEligibleAdjustmentVariables(
    responseVariable = "outcome",
    factorVariables = character(0),
    continuousVariables = c("x", "z")
  )

  selectedAdjustments = sanitizeAdjustmentVariables(
    selectedVariables = character(0),
    eligibleVariables = eligibleAdjustments
  )

  testthat::expect_identical(selectedAdjustments, character(0))

  fittedModel = stats::lm(outcome ~ x + z, data = rawData)
  attr(fittedModel, "wmfm_adjustment_variables") = selectedAdjustments

  roleMetadata = buildEquationDisplayRoleMetadata(fittedModel)

  testthat::expect_false(roleMetadata$hasAdjustments)
  testthat::expect_identical(roleMetadata$adjustmentPredictors, character(0))
  testthat::expect_setequal(roleMetadata$primaryPredictors, c("x", "z"))
  testthat::expect_identical(buildEquationDisplayRoleSummary(roleMetadata), character(0))

  deterministicPrompt = suppressWarnings(lmToExplanationPrompt(fittedModel))
  testthat::expect_no_match(deterministicPrompt, "Adjustment-variable interpretation guidance:", fixed = TRUE)
  testthat::expect_identical(buildAdjustmentVariablePromptBlock(fittedModel), "")
})
