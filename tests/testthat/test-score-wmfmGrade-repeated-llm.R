test_that("score.wmfmGrade stores repeated LLM runs and timing", {
  wm = makeOfflineWmfmModel()

  parsed = makeValidParsedScores()
  parsed$overallScore = 85
  parsed$llmScoringSummary = "Mock score."

  fakeChat = makeFakeChat(
    responseText = jsonlite::toJSON(parsed, auto_unbox = TRUE, null = "null")
  )

  g = grade(
    wm,
    explanation = "Each one-unit increase in x is associated with about a one-point increase in y.",
    autoScore = FALSE
  )

  g = score(g, method = "llm", chat = fakeChat, nLlm = 3, showProgress = FALSE)

  expect_equal(g$scores$byMethod$llm$nRuns, 3)
  expect_length(g$scores$byMethod$llm$runs, 3)
  expect_true(is.numeric(g$scores$byMethod$llm$elapsedSeconds))
  expect_true(is.numeric(g$scores$byMethod$llm$meanSecondsPerRun))
  expect_s3_class(summary(g, method = "llm"), "summary.wmfmGrade")
})
