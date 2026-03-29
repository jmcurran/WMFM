testthat::test_that("compareWmfmScoringMethods summarises paired metrics", {
  x = list(
    runsDf = data.frame(
      overallScore = c(1, 2),
      det_overallScore = c(1, 2),
      llm_overallScore = c(1.5, 1.5),
      overallPass = c(TRUE, TRUE),
      det_overallPass = c(TRUE, TRUE),
      llm_overallPass = c(TRUE, FALSE),
      stringsAsFactors = FALSE
    ),
    primaryScoringMethod = "deterministic",
    opposingScoringMethod = "llm"
  )

  out = compareWmfmScoringMethods(x)

  testthat::expect_true(is.list(out))
  testthat::expect_true("metricAgreement" %in% names(out))
  testthat::expect_true(nrow(out$metricAgreement) >= 1)
})
