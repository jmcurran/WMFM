test_that("deriveMetricComparisonDataFromScores returns paired metric data", {
  scores = makeMockScores(
    detValues = c(0, 1, 2),
    llmValues = c(2, 1, 0)
  )

  out = deriveMetricComparisonDataFromScores(
    scores = scores,
    metric = "numericExpressionAdequate"
  )

  expect_true(is.data.frame(out))
  expect_named(out, c("runId", "detValue", "llmValue"))
  expect_equal(nrow(out), 3)
  expect_equal(out$detValue, c(0, 1, 2))
  expect_equal(out$llmValue, c(2, 1, 0))
})

test_that("deriveMetricComparisonDataFromScores errors when metric missing", {
  scores = makeMockScores(
    detValues = c(0, 1),
    llmValues = c(2, 1),
    metric = "otherMetric"
  )

  expect_error(
    deriveMetricComparisonDataFromScores(
      scores = scores,
      metric = "numericExpressionAdequate"
    ),
    "Metric not found"
  )
})

test_that("deriveMetricComparisonDataFromScores errors when methods are missing", {
  scores = structure(
    list(scores = list(deterministic = list(list(numericExpressionAdequate = 0)))),
    class = "wmfmScores"
  )

  expect_error(
    deriveMetricComparisonDataFromScores(scores, "numericExpressionAdequate"),
    "Both deterministic and llm scores must be present"
  )
})
