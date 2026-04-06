test_that("buildWmfmComparisonPairs builds run-level pair data for available metrics", {

  registry = data.frame(
    metricName = c("m1", "m2"),
    label = c("M1", "M2"),
    group = c("g", "g"),
    metricType = c("binary", "binary"),
    includeInComparison = c(TRUE, TRUE),
    includeInStability = c(TRUE, TRUE),
    includeInPlots = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  registry$orderedLevels = list(NULL, NULL)

  leftDf = data.frame(
    runId = 1:3,
    m1 = c(0, 1, 1),
    stringsAsFactors = FALSE
  )

  rightDf = data.frame(
    runId = 1:3,
    m1 = c(1, 1, 0),
    stringsAsFactors = FALSE
  )

  out = buildWmfmComparisonPairs(
    leftDf = leftDf,
    rightDf = rightDf,
    registry = registry,
    leftMethod = "det",
    rightMethod = "llm"
  )

  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 3)
  expect_true(all(out$metric == "m1"))
  expect_true(all(out$leftMethod == "det"))
  expect_true(all(out$rightMethod == "llm"))
})

test_that("buildWmfmComparisonPairs returns empty data frame when no metrics are shared", {

  registry = data.frame(
    metricName = c("m1", "m2"),
    label = c("M1", "M2"),
    group = c("g", "g"),
    metricType = c("binary", "binary"),
    includeInComparison = c(TRUE, TRUE),
    includeInStability = c(TRUE, TRUE),
    includeInPlots = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  registry$orderedLevels = list(NULL, NULL)

  leftDf = data.frame(
    runId = 1:3,
    m1 = c(0, 1, 1),
    stringsAsFactors = FALSE
  )

  rightDf = data.frame(
    runId = 1:3,
    m2 = c(1, 1, 0),
    stringsAsFactors = FALSE
  )

  out = buildWmfmComparisonPairs(
    leftDf = leftDf,
    rightDf = rightDf,
    registry = registry,
    leftMethod = "det",
    rightMethod = "llm"
  )

  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 0)
})
