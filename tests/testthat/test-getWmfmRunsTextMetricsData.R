testthat::test_that("getWmfmRunsTextMetricsData returns run-level text metrics", {
  x = makeFakeWmfmRuns()

  out = getWmfmRunsTextMetricsData(x)

  testthat::expect_true(is.data.frame(out))
  testthat::expect_identical(names(out), c(
    "runId",
    "wordCount",
    "sentenceCount",
    "runElapsedSeconds"
  ))
  testthat::expect_equal(nrow(out), 3)
})
