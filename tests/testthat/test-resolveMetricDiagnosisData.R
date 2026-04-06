test_that("resolveMetricDiagnosisData adds derived disagreement columns", {
  scores = makeMockScores(
    detValues = c(0, 1, 2),
    llmValues = c(2, 1, 0)
  )

  runs = makeMockRuns(
    explanations = c("exp one", "exp two", "exp three")
  )

  out = resolveMetricDiagnosisData(
    scores = scores,
    metric = "numericExpressionAdequate",
    runs = runs
  )

  expect_true(all(c(
    "detMinusLlm",
    "llmMinusDet",
    "disagrees",
    "disagreementDirection"
  ) %in% names(out)))

  expect_equal(out$llmMinusDet, c(2, 0, -2))
  expect_equal(out$disagrees, c(TRUE, FALSE, TRUE))
  expect_equal(out$disagreementDirection, c("llmHigher", "same", "detHigher"))
})

test_that("resolveMetricDiagnosisData merges contextual columns when available", {
  scores = makeMockScores(
    detValues = c(0, 0),
    llmValues = c(2, 2)
  )

  runs = makeMockRuns(
    explanations = c("exp one", "exp two"),
    effectScaleClaim = c("not_stated", "additive")
  )

  out = resolveMetricDiagnosisData(
    scores = scores,
    metric = "numericExpressionAdequate",
    runs = runs
  )

  expect_true("explanationText" %in% names(out))
  expect_true("effectScaleClaim" %in% names(out))
  expect_equal(out$explanationText, c("exp one", "exp two"))
})

test_that("resolveMetricDiagnosisData works without runs", {
  scores = makeMockScores(
    detValues = c(0, 0),
    llmValues = c(2, 2)
  )

  out = resolveMetricDiagnosisData(
    scores = scores,
    metric = "numericExpressionAdequate",
    runs = NULL
  )

  expect_true(is.data.frame(out))
  expect_false("explanationText" %in% names(out))
})
