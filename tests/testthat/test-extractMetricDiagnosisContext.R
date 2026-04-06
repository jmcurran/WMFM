test_that("extractMetricDiagnosisContext returns runsDf context when present", {
  runs = makeMockRuns(
    explanations = c("a", "b", "c"),
    effectScaleClaim = c("not_stated", "additive", "multiplicative"),
    percentLanguageMention = c(FALSE, FALSE, TRUE)
  )

  scores = makeMockScores(
    detValues = c(0, 1, 2),
    llmValues = c(2, 2, 2)
  )

  out = extractMetricDiagnosisContext(
    scores = scores,
    metric = "numericExpressionAdequate",
    runs = runs
  )

  expect_true(is.data.frame(out))
  expect_true(all(c("runId", "explanationText", "effectScaleClaim") %in% names(out)))
})

test_that("extractMetricDiagnosisContext returns NULL when no context is available", {
  scores = makeMockScores(
    detValues = c(0, 1),
    llmValues = c(2, 1)
  )

  out = extractMetricDiagnosisContext(
    scores = scores,
    metric = "numericExpressionAdequate",
    runs = NULL
  )

  expect_null(out)
})
