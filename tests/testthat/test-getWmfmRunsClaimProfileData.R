testthat::test_that("getWmfmRunsClaimProfileData reshapes raw claim fields into long format", {
  x = makeFakeWmfmRuns()

  out = getWmfmRunsClaimProfileData(x)

  testthat::expect_true(is.data.frame(out))
  testthat::expect_true(all(c(
    "runId",
    "field",
    "fieldLabel",
    "value",
    "modalValue",
    "fieldPurity",
    "runPurity"
  ) %in% names(out)))
  testthat::expect_true(all(out$runId %in% c(1L, 2L, 3L)))
})
