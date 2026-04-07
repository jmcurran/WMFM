test_that("full pipeline runs -> score -> compare -> diagnose works end-to-end", {

  # --- Step 1: runs ---
  runs = makeFakeWmfmRuns(
    explanations = c(
      "Exam scores increase by about 3.5 points with attendance.",
      "Attendance is linked to higher scores, roughly three points more.",
      "Students who attend score about 3.5 points higher on average."
    )
  )

  expect_s3_class(runs, "wmfmRuns")

  # --- Step 2: deterministic scoring ---
  scores = score(runs, method = "deterministic")

  expect_s3_class(scores, "wmfmScores")
  expect_true("deterministic" %in% scores$methods)
  expect_equal(length(scores$scores$deterministic), length(runs$runs))

  # --- Step 3: comparison (det vs det as sanity baseline) ---
  cmp = compare(scores, scores)

  expect_s3_class(cmp, "wmfmScoreComparison")
  expect_true(nrow(cmp$pairData) > 0)

  # --- Step 4: diagnose ---
  dx = diagnose(scores)

  expect_s3_class(dx, "wmfmScoresDiagnosis")

  expect_true(nrow(dx$summaryTable) > 0)
  expect_true(is.data.frame(dx$flaggedMetrics))
  expect_true(is.list(dx$metricDiagnoses))

  # --- Step 5: sanity checks on outputs ---
  expect_true(all(c(
    "metric",
    "diagnosisClass",
    "priorityScore"
  ) %in% names(dx$flaggedMetrics)))

})

test_that("full pipeline detects disagreement when scores differ", {

  # --- runs ---
  runs = makeFakeWmfmRuns()

  # --- score ---
  scores = score(runs, method = "deterministic")

  # --- manually inject disagreement ---
  scores$scores$llm = lapply(scores$scores$deterministic, function(x) {
    x2 = x
    if (!is.null(x2$numericExpressionAdequate)) {
      x2$numericExpressionAdequate = 2
    }
    x2
  })
  scores$methods = c("deterministic", "llm")

  # --- diagnose ---
  dx = diagnose(scores)

  expect_s3_class(dx, "wmfmScoresDiagnosis")

  # --- key behavioural checks ---
  expect_true(nrow(dx$flaggedMetrics) > 0)

  expect_true(any(
    dx$flaggedMetrics$diagnosisClass != "noSystematicDisagreement"
  ))

})
