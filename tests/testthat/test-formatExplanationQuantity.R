test_that("formatExplanationNumber uses two significant figures by default", {
  expect_identical(formatExplanationNumber(0.214), "0.21")
  expect_identical(formatExplanationNumber(6.78), "6.8")
  expect_identical(formatExplanationNumber(1234.56), "1200")
  expect_identical(formatExplanationNumber(0.004321), "0.0043")
})

test_that("formatExplanationNumber handles integer-like values and missing values", {
  expect_identical(formatExplanationNumber(10.0312), "10")
  expect_identical(formatExplanationNumber(NA_real_), NA_character_)
})

test_that("formatExplanationConfidenceInterval formats estimate and bounds consistently", {
  out = formatExplanationConfidenceInterval(
    estimate = 6.783,
    lower = 5.214,
    upper = 8.392
  )

  expect_identical(out$estimate, "6.8")
  expect_identical(out$lower, "5.2")
  expect_identical(out$upper, "8.4")
  expect_identical(out$confidenceLevel, "95%")
  expect_true(grepl("confidence interval from 5.2 to 8.4", out$text, fixed = TRUE))
})

test_that("formatExplanationProbability formats ordinary and boundary probabilities", {
  expect_identical(formatExplanationProbability(0.123), "12%")
  expect_identical(formatExplanationProbability(0.995), ">99%")
  expect_identical(formatExplanationProbability(0.004), "<1%")
  expect_identical(formatExplanationProbability(0.123, asPercent = FALSE), "0.12")
})

test_that("formatExplanationOdds keeps odds as odds", {
  expect_identical(formatExplanationOdds(0.7), "0.7:1")
  expect_identical(formatExplanationOdds(2.5), "2.5:1")
  expect_identical(formatExplanationOdds(0.001), "1:1000")
  expect_identical(formatExplanationOdds(0), "0:1")
})

test_that("formatExplanationMultiplier and formatExplanationAnchor use explanation number formatting", {
  expect_identical(formatExplanationMultiplier(0.214), "0.21")
  expect_identical(formatExplanationAnchor(6.247368), "6.25")
})

test_that("formatExplanationQuantity routes common quantity types", {
  expect_identical(formatExplanationQuantity(0.123, quantityType = "probability"), "12%")
  expect_identical(formatExplanationQuantity(0.001, quantityType = "odds"), "1:1000")
  expect_identical(formatExplanationQuantity(1.567, quantityType = "multiplier"), "1.6")
  expect_identical(formatExplanationQuantity(11.567, quantityType = "number"), "12")
  expect_identical(formatExplanationQuantity(11.567, quantityType = "anchor"), "11.6")
})

test_that("formatExplanationQuantity validates impossible values", {
  expect_error(formatExplanationProbability(1.2), "between 0 and 1", fixed = TRUE)
  expect_error(formatExplanationOdds(-0.5), "non-negative", fixed = TRUE)
  expect_error(formatExplanationNumber("x"), "must be numeric", fixed = TRUE)
})
