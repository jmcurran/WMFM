test_that("summarizeOrdinalAgreement returns expected ordinal summary", {
  metricRow = makeMetricRow("clarityAdequate")

  out = summarizeOrdinalAgreement(
    leftVec = c("inadequate", "mixed_or_unclear", "adequate"),
    rightVec = c("mixed_or_unclear", "mixed_or_unclear", "adequate"),
    metricRow = metricRow
  )

  expect_s3_class(out, "data.frame")
  expect_equal(out$metric, "clarityAdequate")
  expect_equal(out$nCompared, 3)
  expect_equal(out$proportionEqual, 2 / 3)
  expect_equal(out$proportionAdjacent, 1)
  expect_equal(out$meanDifference, 1 / 3)
  expect_equal(out$meanAbsoluteDifference, 1 / 3)
  expect_true(is.finite(out$weightedKappa))
})

test_that("summarizeOrdinalAgreement ignores values not in orderedLevels", {
  metricRow = makeMetricRow("clarityAdequate")

  out = summarizeOrdinalAgreement(
    leftVec = c("adequate", "not_a_level"),
    rightVec = c("adequate", "adequate"),
    metricRow = metricRow
  )

  expect_equal(out$nCompared, 1)
  expect_equal(out$proportionEqual, 1)
})
