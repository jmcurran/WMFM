test_that("full pipeline runs -> score -> diagnose works end-to-end", {

  runs = makeFakeWmfmRuns(
    explanations = c(
      "Exam scores increase by about 3.5 points with attendance.",
      "Attendance is linked to higher scores, roughly three points more.",
      "Students who attend score about 3.5 points higher on average."
    )
  )

  expect_s3_class(runs, "wmfmRuns")

  scores = score(runs, method = "deterministic")

  # inject llm method
  scores$scores$llm = lapply(scores$scores$deterministic, function(x) {
    x2 = x
    if (!is.null(x2$overallScore)) {
      x2$overallScore = x2$overallScore + 0.1
    }
    x2
  })
  scores$methods = c("deterministic", "llm")

  expect_s3_class(scores, "wmfmScores")
  expect_true(all(c("deterministic", "llm") %in% scores$methods))

  # diagnose (suppress repeated warnings)
  dx = suppressWarnings(diagnose(scores))

  expect_s3_class(dx, "wmfmScoresDiagnosis")
  expect_true(nrow(dx$summaryTable) > 0)
  expect_true(is.data.frame(dx$flaggedMetrics))
  expect_true(is.list(dx$metricDiagnoses))
  expect_true(all(c(
    "metric",
    "diagnosisClass",
    "priorityScore"
  ) %in% names(dx$flaggedMetrics)))
})

test_that("full pipeline detects disagreement when scores differ", {

  runs = makeFakeWmfmRuns()
  scores = score(runs, method = "deterministic")

  scores$scores$llm = lapply(scores$scores$deterministic, function(x) {
    x2 = x
    if (!is.null(x2$numericExpressionAdequate)) {
      x2$numericExpressionAdequate = 2
    }
    x2
  })
  scores$methods = c("deterministic", "llm")

  dx = suppressWarnings(diagnose(scores))

  expect_s3_class(dx, "wmfmScoresDiagnosis")
  expect_true(nrow(dx$flaggedMetrics) > 0)
  expect_true(any(
    dx$flaggedMetrics$diagnosisClass != "noSystematicDisagreement"
  ))
})

test_that("diagnose errors cleanly for single-method scores objects", {
  runs = makeFakeWmfmRuns()
  scores = score(runs, method = "deterministic")

  expect_error(
    diagnose(scores),
    "Both deterministic and llm scores must be present",
    ignore.case = TRUE
  )
})
