testthat::test_that("evaluation examples expose Stage 43.11 metadata", {
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
