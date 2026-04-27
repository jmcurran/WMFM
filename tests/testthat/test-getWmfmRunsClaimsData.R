testthat::test_that("getWmfmRunsClaimsData returns one row per binary claim field", {
  x = makeFakeWmfmRuns()

  out = getWmfmRunsClaimsData(x)

  testthat::expect_true(is.data.frame(out))
  testthat::expect_true(all(c("claim", "nPresent", "nRuns", "proportionPresent") %in% names(out)))
  testthat::expect_true(nrow(out) >= 10)
  testthat::expect_true(all(out$nRuns == 3L))
})
