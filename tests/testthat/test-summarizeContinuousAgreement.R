test_that("summarizeContinuousAgreement returns expected continuous summary", {
  metricRow = makeMetricRow("overallScore")

  out = summarizeContinuousAgreement(
    leftVec = c(1, 2, 3),
    rightVec = c(2, 2, 4),
    metricRow = metricRow
  )

  expect_s3_class(out, "data.frame")
  expect_equal(out$metric, "overallScore")
  expect_equal(out$nCompared, 3)
  expect_equal(out$meanDifference, 2 / 3)
  expect_equal(out$meanAbsoluteDifference, 2 / 3)
  expect_true(is.finite(out$correlation))
  expect_true(is.finite(out$sdDifference))
  expect_equal(out$loaLower, out$meanDifference - 1.96 * out$sdDifference)
  expect_equal(out$loaUpper, out$meanDifference + 1.96 * out$sdDifference)
})

test_that("summarizeContinuousAgreement returns NA correlation and sd for one pair", {
  metricRow = makeMetricRow("overallScore")

  out = summarizeContinuousAgreement(
    leftVec = 1,
    rightVec = 2,
    metricRow = metricRow
  )

  expect_true(is.na(out$correlation))
  expect_true(is.na(out$sdDifference))
})
