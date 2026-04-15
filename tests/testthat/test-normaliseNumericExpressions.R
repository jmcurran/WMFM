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


test_that("normaliseNumericExpressions rewrites malformed hyphenated percentages", {
  expect_equal(
    normaliseNumericExpressions(
      paste(
        "The expected count fell to about twenty-1 percent of its previous level,",
        "with values from about one-five to thirty-1 percent still consistent with the data."
      )
    ),
    paste(
      "The expected count fell to about 21 percent of its previous level,",
      "with values from about 15 to 31 percent still consistent with the data."
    )
  )
})

test_that("normaliseNumericExpressions rewrites malformed hyphenated percentages with Unicode hyphens", {
  expect_equal(
    normaliseNumericExpressions("The effect was about four‑2 percent."),
    "The effect was about 42 percent."
  )
})
