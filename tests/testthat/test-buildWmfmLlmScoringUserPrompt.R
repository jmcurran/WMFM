makeRunRecordForPrompt = function() {
  list(
    exampleName = "Course",
    package = "WMFM",
    modelType = "lm",
    formula = "Exam ~ Attend + Test",
    equationsText = "Exam = 6.62 + 3.52 * Test",
    explanationText = "The model suggests higher scores with attendance.",
    interactionTerms = "",
    hasInteractionTerms = FALSE,
    nInteractionTerms = 0L,
    interactionMinPValue = NA_real_,
    interactionAlpha = 0.05
  )
}

testthat::test_that("buildWmfmLlmScoringUserPrompt includes key context", {
  prompt = buildWmfmLlmScoringUserPrompt(makeRunRecordForPrompt())

  testthat::expect_match(prompt, "MODEL CONTEXT")
  testthat::expect_match(prompt, "EXPLANATION TO SCORE")
  testthat::expect_match(prompt, "effectDirectionCorrect")
})

testthat::test_that("buildWmfmLlmScoringUserPrompt validates required fields", {
  x = makeRunRecordForPrompt()
  x$formula = NULL

  testthat::expect_error(
    buildWmfmLlmScoringUserPrompt(x),
    "missing required fields"
  )
})
