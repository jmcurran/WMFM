testthat::test_that("prediction evaluation examples expose intent metadata", {
  examples = listWMFMEvaluationExamples(package = "WMFM")
  selected = examples[examples$suite == "prediction_core", , drop = FALSE]

  testthat::expect_equal(nrow(selected), 12)
  testthat::expect_setequal(selected$datasetGroup, c("course", "oysters", "quakes", "diamonds"))
  testthat::expect_setequal(
    selected$intendedIntent,
    c("individual_outcome", "ambiguous_personal", "mean_response")
  )
})

testthat::test_that("evaluation runner requires exactly one selector", {
  outputDir = tempfile("wmfm-evaluation-")

  testthat::expect_error(
    runWMFMEvaluationSuite(outputDir = outputDir),
    "exactly one"
  )
  testthat::expect_error(
    runWMFMEvaluationSuite(
      pattern = "Prediction",
      suite = "prediction_core",
      outputDir = outputDir
    ),
    "exactly one"
  )
})


testthat::test_that("developer observation examples are available to the evaluation suite", {
  examples = listWMFMEvaluationExamples(package = "WMFM")
  selected = examples[examples$suite == "observation_questions", , drop = FALSE]

  expectedNames = c(
    "Residual Observations Below Expected",
    "Residual Observations Above Expected",
    "Most Unusual Residual Observations",
    "Comparable Diamond Observations",
    "Conditional Diamond Price Percentile",
    "Unsupported Poisson Residual Inspection"
  )

  testthat::expect_equal(nrow(selected), 6L)
  testthat::expect_setequal(selected$name, expectedNames)
  testthat::expect_true(all(selected$taskType == "observation"))
})

testthat::test_that("classroom evaluation example numbering remains stable", {
  examples = listWMFMEvaluationExamples(package = "WMFM")
  classroomNames = listWMFMExamples(package = "WMFM")

  testthat::expect_identical(examples$name[seq_along(classroomNames)], classroomNames)
  testthat::expect_identical(examples$number[seq_along(classroomNames)], seq_along(classroomNames))
})

testthat::test_that("evaluation catalogue metadata does not require loading example data", {
  testthat::expect_no_error(
    listWMFMEvaluationExamples(
      package = "WMFM",
      includeTestExamples = TRUE
    )
  )
})
