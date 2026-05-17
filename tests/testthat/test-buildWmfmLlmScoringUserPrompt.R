testthat::test_that("buildWmfmLlmScoringUserPrompt includes key scoring context", {
  prompt = buildWmfmLlmScoringUserPrompt(makeRawRunRecordForScoring())

  testthat::expect_match(prompt, "MODEL CONTEXT")
  testthat::expect_match(prompt, "EXPLANATION TO SCORE")
  testthat::expect_match(prompt, "effectDirectionCorrect")
  testthat::expect_match(prompt, "fieldReasons")
})

testthat::test_that("buildWmfmLlmScoringUserPrompt validates required fields", {
  x = makeRawRunRecordForScoring()
  x$formula = NULL

  testthat::expect_error(
    buildWmfmLlmScoringUserPrompt(x),
    "missing required fields"
  )
})


testthat::test_that("buildWmfmLlmScoringUserPrompt reports interaction context when present", {
  x = makeRawRunRecordForScoring(
    interactionTerms = c("Attend:Group"),
    interactionMinPValue = 0.012
  )

  prompt = buildWmfmLlmScoringUserPrompt(x)

  testthat::expect_match(prompt, "Interaction terms are present")
  testthat::expect_match(prompt, "Attend:Group")
  testthat::expect_match(prompt, "0.012")
})

testthat::test_that("buildWmfmLlmScoringUserPrompt includes adjustment-aware scoring policy when supplied", {
  x = makeRawRunRecordForScoring()
  x$adjustmentVariables = "picture"
  x$primaryVariables = "gender"

  prompt = buildWmfmLlmScoringUserPrompt(x)

  testthat::expect_match(prompt, "ADJUSTMENT CONTEXT", fixed = TRUE)
  testthat::expect_match(prompt, "Adjustment variables: picture", fixed = TRUE)
  testthat::expect_match(prompt, "Variables of scientific interest: gender", fixed = TRUE)
  testthat::expect_match(prompt, "Do not penalise explanations for omitting adjustment-level details", fixed = TRUE)
  testthat::expect_match(prompt, "do not penalise the absence of interaction cell-by-cell descriptions", ignore.case = TRUE)
})

testthat::test_that("buildWmfmLlmScoringUserPrompt keeps default guidance when no adjustment variables are supplied", {
  prompt = buildWmfmLlmScoringUserPrompt(makeRawRunRecordForScoring())

  testthat::expect_match(prompt, "ADJUSTMENT CONTEXT", fixed = TRUE)
  testthat::expect_match(prompt, "No adjustment-variable context was supplied", fixed = TRUE)
})
