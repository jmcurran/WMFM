testthat::test_that("adjustment scoring policy normalizes supplied role variables", {
  runRecord = makeRawRunRecordForScoring(
    adjustmentVariables = c(" picture ", "picture", ""),
    primaryVariables = c("gender", " gender ")
  )

  policy = buildAdjustmentScoringPolicy(runRecord = runRecord)

  testthat::expect_identical(policy$adjustmentVariables, "picture")
  testthat::expect_identical(policy$primaryVariables, "gender")
  testthat::expect_true(policy$hasAdjustmentVariables)
  testthat::expect_true(policy$hasPrimaryVariables)
})

testthat::test_that("adjustment scoring policy keeps restrained-governance wording centralized", {
  runRecord = makeRawRunRecordForScoring(
    adjustmentVariables = "picture",
    primaryVariables = "gender"
  )

  policy = buildAdjustmentScoringPolicy(runRecord = runRecord)
  contextBlock = buildAdjustmentScoringContextBlock(runRecord = runRecord, policy = policy)

  testthat::expect_match(contextBlock, "Adjustment variables: picture", fixed = TRUE)
  testthat::expect_match(contextBlock, "Variables of scientific interest: gender", fixed = TRUE)
  testthat::expect_match(contextBlock, "Do not penalise explanations for omitting adjustment-level details", fixed = TRUE)
  testthat::expect_match(contextBlock, "should not become narrative conditioning axes", fixed = TRUE)
  testthat::expect_match(contextBlock, "recite the regression table instead of answering the research question", fixed = TRUE)
})

testthat::test_that("scoring prompt uses centralized adjustment context block", {
  runRecord = makeRawRunRecordForScoring(
    formula = "arousal ~ gender + picture + gender:picture",
    interactionTerms = "gender:picture",
    adjustmentVariables = "picture",
    primaryVariables = "gender"
  )

  expectedBlock = buildAdjustmentScoringContextBlock(runRecord = runRecord)
  prompt = buildWmfmLlmScoringUserPrompt(runRecord)

  testthat::expect_match(prompt, "ADJUSTMENT CONTEXT", fixed = TRUE)
  testthat::expect_match(prompt, expectedBlock, fixed = TRUE)
})

testthat::test_that("scoring prompt retains standard adjustment wording when no adjustment variables are supplied", {
  runRecord = makeRawRunRecordForScoring()

  expectedBlock = buildAdjustmentScoringContextBlock(runRecord = runRecord)
  prompt = buildWmfmLlmScoringUserPrompt(runRecord)

  testthat::expect_match(prompt, "ADJUSTMENT CONTEXT", fixed = TRUE)
  testthat::expect_match(prompt, expectedBlock, fixed = TRUE)
  testthat::expect_match(prompt, "No adjustment-variable context was supplied", fixed = TRUE)
})
