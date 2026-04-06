test_that("summarizeOrdinalAgreement returns expected ordinal summary", {

  metricRow = data.frame(
    metricName = "clarityAdequate",
    label = "Clarity",
    group = "g",
    metricType = "ordinal",
    stringsAsFactors = FALSE
  )

  metricRow$orderedLevels = list(c("0", "1", "2"))

  left = c("0", "1", "2")
  right = c("0", "2", "2")

  out = summarizeOrdinalAgreement(left, right, metricRow)

  expect_true(is.data.frame(out))
  expect_equal(out$metric, "clarityAdequate")
  expect_equal(out$nCompared, 3)
  expect_equal(out$proportionEqual, 2/3)
  expect_equal(out$proportionAdjacent, 1)
})

test_that("summarizeOrdinalAgreement returns NULL when values not in orderedLevels", {

  metricRow = data.frame(
    metricName = "clarityAdequate",
    label = "Clarity",
    group = "g",
    metricType = "ordinal",
    stringsAsFactors = FALSE
  )

  metricRow$orderedLevels = list(c("0", "1", "2"))

  left = c("A", "B")
  right = c("A", "B")

  out = summarizeOrdinalAgreement(left, right, metricRow)

  expect_null(out)
})
