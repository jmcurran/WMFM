testthat::test_that("plot.wmfmRuns returns ggplot objects for each supported type", {
  x = makeFakeWmfmRuns()

  p1 = plot(x, type = "claims")
  p2 = plot(x, type = "textMetrics")
  p3 = plot(x, type = "claimProfile")

  testthat::expect_s3_class(p1, "ggplot")
  testthat::expect_s3_class(p2, "ggplot")
  testthat::expect_s3_class(p3, "ggplot")
})
