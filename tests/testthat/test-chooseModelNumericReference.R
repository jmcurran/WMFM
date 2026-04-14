testthat::test_that("chooseModelNumericReference uses zero when all numeric predictors include zero", {

  d = data.frame(
    y = c(1, 2, 3, 4),
    x = c(-1, 0, 1, 2),
    z = c(-2, -1, 0, 3)
  )

  out = chooseModelNumericReference(mf = d)

  testthat::expect_identical(out, "zero")
})

testthat::test_that("chooseModelNumericReference uses mean when any numeric predictor excludes zero", {

  d = data.frame(
    y = c(1, 2, 3, 4),
    x = c(5.25, 5.50, 5.75, 6.00),
    g = factor(c("a", "a", "b", "b"))
  )

  out = chooseModelNumericReference(mf = d)

  testthat::expect_identical(out, "mean")
})
