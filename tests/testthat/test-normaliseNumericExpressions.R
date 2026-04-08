test_that("normaliseNumericExpressions rewrites simple half expressions", {
  expect_equal(
    normaliseNumericExpressions("The increase was about six and a half points."),
    "The increase was about 6.5 points."
  )
})

test_that("normaliseNumericExpressions rewrites worded percentages", {
  expect_equal(
    normaliseNumericExpressions("About sixty-two percent of the variation was explained."),
    "About 62 percent of the variation was explained."
  )
})

test_that("normaliseNumericExpressions preserves existing numeral formatting", {
  expect_equal(
    normaliseNumericExpressions("The effect was 3.5 points and 62% of the variation was explained."),
    "The effect was 3.5 points and 62% of the variation was explained."
  )
})

test_that("normaliseNumericExpressions can handle multiple replacements", {
  expect_equal(
    normaliseNumericExpressions(
      paste(
        "The intercept was about six and a half points,",
        "and roughly sixty-two percent of the variation was explained."
      )
    ),
    paste(
      "The intercept was about 6.5 points,",
      "and roughly 62 percent of the variation was explained."
    )
  )
})
