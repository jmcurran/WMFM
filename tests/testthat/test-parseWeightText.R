test_that("parseWeightText parses decimals", {
  expect_equal(parseWeightText("0.5"), 0.5)
  expect_equal(parseWeightText(" 2 "), 2)
  expect_equal(parseWeightText("-1.25"), -1.25)
})

test_that("parseWeightText parses simple fractions", {
  expect_equal(parseWeightText("1/2"), 0.5)
  expect_equal(parseWeightText(" 3 / 4 "), 0.75)
  expect_equal(parseWeightText("-1/2"), -0.5)
})

test_that("parseWeightText returns NA_real_ on invalid input", {
  expect_true(is.na(parseWeightText(NULL)))
  expect_true(is.na(parseWeightText("")))
  expect_true(is.na(parseWeightText("not a number")))
  expect_true(is.na(parseWeightText("1/0")))
  expect_true(is.na(parseWeightText("1/")))
  expect_true(is.na(parseWeightText("/2")))
  expect_true(is.na(parseWeightText("1/2/3")))
})
