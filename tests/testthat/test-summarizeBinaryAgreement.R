test_that("summarizeBinaryAgreement returns expected binary summary", {

  metricRow = data.frame(
    metricName = "m1",
    label = "M1",
    group = "g",
    metricType = "binary",
    stringsAsFactors = FALSE
  )

  left = c(TRUE, TRUE, FALSE)
  right = c(TRUE, FALSE, FALSE)

  out = summarizeBinaryAgreement(left, right, metricRow)

  expect_true(is.data.frame(out))
  expect_equal(out$nCompared, 3)
  expect_equal(out$proportionEqual, 2/3)
  expect_equal(out$trueRateLeft, 2/3)
  expect_equal(out$trueRateRight, 1/3)
})

test_that("summarizeBinaryAgreement returns NULL when no valid pairs", {

  metricRow = data.frame(
    metricName = "m1",
    label = "M1",
    group = "g",
    metricType = "binary",
    stringsAsFactors = FALSE
  )

  left = c(NA, NA)
  right = c(NA, NA)

  out = summarizeBinaryAgreement(left, right, metricRow)

  expect_null(out)
})
