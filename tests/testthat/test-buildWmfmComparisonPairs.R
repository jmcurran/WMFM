test_that("buildWmfmComparisonPairs builds run-level pair data", {
  longDf = makeLongScoreDf()
  leftDf = subset(longDf, method == "deterministic")
  rightDf = subset(longDf, method == "llm")
  registry = getWmfmMetricRegistry()

  out = buildWmfmComparisonPairs(
    leftDf = leftDf,
    rightDf = rightDf,
    registry = registry,
    leftMethod = "deterministic",
    rightMethod = "llm"
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3 * nrow(registry[registry$includeInComparison, , drop = FALSE]))
  expect_true(all(c(
    "runId",
    "metric",
    "label",
    "group",
    "metricType",
    "leftMethod",
    "rightMethod",
    "leftValue",
    "rightValue"
  ) %in% names(out)))

  overallRows = out[out$metric == "overallScore", , drop = FALSE]
  expect_equal(nrow(overallRows), 3)
  expect_equal(as.numeric(overallRows$leftValue), c(1, 2, 3))
  expect_equal(as.numeric(overallRows$rightValue), c(1.2, 1.8, 3.2))
})
