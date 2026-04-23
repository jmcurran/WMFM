test_that("diagnose returns wmfmMetricDiagnosis for single metric", {
  scores = makeDiagnoseScoresFixture()

  dx = NULL

  expect_warning(
    dx <- diagnose(scores, metric = "factualScore"),
    regexp = "No `runs` object supplied",
    ignore.case = TRUE
  )

  expect_s3_class(dx, "wmfmMetricDiagnosis")
  expect_true(all(c(
    "summary",
    "evidenceSummary",
    "runDiagnosis",
    "flaggedRuns"
  ) %in% names(dx)))
})

test_that("diagnose returns wmfmScoresDiagnosis for all metrics", {
  scores = makeDiagnoseScoresFixture()

  dx = NULL

  expect_warning(
    dx <- diagnose(scores),
    regexp = "No `runs` object supplied",
    ignore.case = TRUE
  )

  expect_s3_class(dx, "wmfmScoresDiagnosis")
  expect_true(all(c(
    "summaryTable",
    "flaggedMetrics",
    "metricDiagnoses"
  ) %in% names(dx)))
})
