testthat::test_that("scoreWmfmRunRecordByMethod handles none", {
  x = list(explanationText = "abc")
  out = scoreWmfmRunRecordByMethod(x, scoringMethod = "none")
  testthat::expect_identical(out$primaryScoringMethod, "none")
})
