test_that("integer values are shown without decimal places", {
  expect_equal(formatSummaryValue(11), "11")
  expect_equal(formatSummaryValue(93), "93")
  expect_equal(formatSummaryValue(-4), "-4")
})

test_that("non-integer values use significant digits", {
  expect_equal(formatSummaryValue(52.8767), "52.9")
  expect_equal(formatSummaryValue(18.678), "18.7")
  expect_equal(formatSummaryValue(68.5), "68.5")
})

test_that("the number of significant digits can be changed", {
  expect_equal(formatSummaryValue(52.8767, sigDigits = 2), "53")
  expect_equal(formatSummaryValue(0.012345, sigDigits = 2), "0.012")
})

test_that("missing and empty inputs return NA_character_", {
  expect_equal(formatSummaryValue(NA_real_), NA_character_)
  expect_equal(formatSummaryValue(numeric(0)), NA_character_)
})

test_that("invalid inputs error cleanly", {
  expect_error(formatSummaryValue("abc"), "x must be numeric")
  expect_error(formatSummaryValue(c(1, 2)), "x must be a numeric scalar")
  expect_error(formatSummaryValue(1.23, sigDigits = 0), "sigDigits must be at least 1")
  expect_error(
    formatSummaryValue(1.23, sigDigits = NA_real_),
    "sigDigits must be a single non-missing numeric value"
  )
})
