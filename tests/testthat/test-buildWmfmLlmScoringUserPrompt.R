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
