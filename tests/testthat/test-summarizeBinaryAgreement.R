test_that("summarizeBinaryAgreement returns expected binary summary", {
  metricRow = makeMetricRow("uncertaintyAppropriate")

  out = summarizeBinaryAgreement(
    leftVec = c(TRUE, FALSE, TRUE),
    rightVec = c(TRUE, TRUE, FALSE),
    metricRow = metricRow
  )

  expect_s3_class(out, "data.frame")
  expect_equal(out$metric, "uncertaintyAppropriate")
  expect_equal(out$nCompared, 3)
  expect_equal(out$proportionEqual, 1 / 3)
  expect_equal(out$trueRateLeft, 2 / 3)
  expect_equal(out$trueRateRight, 2 / 3)
})

test_that("summarizeBinaryAgreement returns NULL when no complete pairs exist", {
  metricRow = makeMetricRow("uncertaintyAppropriate")

  expect_null(
    summarizeBinaryAgreement(
      leftVec = c(NA, NA),
      rightVec = c(NA, NA),
      metricRow = metricRow
    )
  )
})
