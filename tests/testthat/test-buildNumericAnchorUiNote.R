testthat::test_that("buildNumericAnchorUiNote returns detailed mean-based note when zero is outside range", {

  d = data.frame(
    y = c(1, 2, 3, 4),
    x = c(5.25, 5.50, 5.75, 6.00),
    g = factor(c("a", "a", "b", "b"))
  )

  out = buildNumericAnchorUiNote(mf = d)

  testthat::expect_match(out, "sample means unless stated otherwise", fixed = TRUE)
  testthat::expect_match(out, "x = 5.625", fixed = TRUE)
  testthat::expect_match(out, "observed range [5.25, 6]", fixed = TRUE)
})

testthat::test_that("buildNumericAnchorUiNote returns detailed zero-based note when zero is inside range", {

  d = data.frame(
    y = c(1, 2, 3, 4),
    x = c(-1, 0, 1, 2)
  )

  out = buildNumericAnchorUiNote(mf = d)

  testthat::expect_match(out, "described at 0", fixed = TRUE)
  testthat::expect_match(out, "x observed range [-1, 2]", fixed = TRUE)
})

testthat::test_that("buildNumericAnchorUiNote returns empty text when there are no numeric predictors", {

  d = data.frame(
    y = c(1, 2, 3, 4),
    g = factor(c("a", "a", "b", "b"))
  )

  out = buildNumericAnchorUiNote(mf = d)

  testthat::expect_identical(out, "")
})

testthat::test_that("buildNumericAnchorUiNote includes all numeric predictors in the detailed note", {

  d = data.frame(
    y = c(1, 2, 3, 4),
    x = c(10, 12, 14, 16),
    z = c(2, 4, 6, 8)
  )

  out = buildNumericAnchorUiNote(mf = d)

  testthat::expect_match(out, "x = 13", fixed = TRUE)
  testthat::expect_match(out, "z = 5", fixed = TRUE)
})
