testthat::test_that("print.summary.wmfmRuns prints without error", {
  x = makeFakeWmfmRuns()
  sx = summary(x)

  testthat::expect_invisible(print(sx))
})
