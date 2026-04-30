test_that("score.wmfmGrade guards repeated LLM grading jobs", {
  wm = makeOfflineWmfmModel()

  g = grade(
    wm,
    explanation = "Each one-unit increase in x is associated with about a one-point increase in y.",
    autoScore = FALSE
  )

  expect_error(
    score(
      g,
      method = "llm",
      chat = makeFakeChat("{}"),
      nLlm = 3,
      maxLlmJobsWithoutConfirmation = 2,
      showProgress = FALSE
    ),
    "Set `confirmLargeLlmJob = TRUE` to proceed"
  )
})

test_that("score.wmfmGrade records confirmed repeated LLM grading metadata", {
  wm = makeOfflineWmfmModel()

  parsed = makeValidParsedScores()
  parsed$overallScore = 82
  parsed$llmScoringSummary = "Mock score."

  fakeChat = makeFakeChat(
    responseText = jsonlite::toJSON(parsed, auto_unbox = TRUE, null = "null")
  )

  g = grade(
    wm,
    explanation = "Each one-unit increase in x is associated with about a one-point increase in y.",
    autoScore = FALSE
  )

  out = score(
    g,
    method = "llm",
    chat = fakeChat,
    nLlm = 3,
    confirmLargeLlmJob = TRUE,
    maxLlmJobsWithoutConfirmation = 2,
    showProgress = FALSE
  )

  expect_equal(out$scores$byMethod$llm$nRuns, 3)
  expect_equal(out$meta$totalLlmCalls, 3)
  expect_true(out$meta$confirmLargeLlmJob)
  expect_equal(out$meta$maxLlmJobsWithoutConfirmation, 2)
})
