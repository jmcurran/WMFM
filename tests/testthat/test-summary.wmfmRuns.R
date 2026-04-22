testthat::test_that("summary.wmfmRuns returns raw-run summary only", {
  x = makeFakeWmfmRuns(
    explanations = c(
      "Attendance is associated with higher exam scores.",
      "Attendance is associated with higher exam scores.",
      "The data suggest exam scores are higher with attendance."
    )
  )

  out = summary(x)

  testthat::expect_s3_class(out, "summary.wmfmRuns")
  testthat::expect_identical(out$nRuns, 3L)
  testthat::expect_true("claimSummary" %in% names(out))
  testthat::expect_false(any(c("overallScore", "llmScored") %in% names(out)))
})
