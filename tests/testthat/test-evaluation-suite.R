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


testthat::test_that("evaluation summaries use follow-up categories as detected intent", {
  observationResult = list(
    diagnostics = list(followupCategory = "observation_residual_request")
  )
  predictionResult = list(
    diagnostics = list(
      predictionPayload = list(predictionIntent = "individual_outcome")
    )
  )
  objectiveResult = list(
    diagnostics = list(
      researchQuestionObjective = list(
        archetype = "individual_prediction",
        route = "model_answer",
        requiresFollowup = TRUE
      )
    )
  )

  testthat::expect_identical(
    getWMFMEvaluationDetectedIntent(observationResult),
    "observation_residual_request"
  )
  testthat::expect_identical(
    getWMFMEvaluationDetectedIntent(predictionResult),
    "individual_outcome"
  )
  testthat::expect_identical(
    getWMFMEvaluationDetectedIntent(objectiveResult),
    "individual_prediction"
  )
  testthat::expect_identical(
    getWMFMEvaluationObservedRoute(objectiveResult),
    "model_answer"
  )
  testthat::expect_true(
    getWMFMEvaluationRequiresFollowup(objectiveResult)
  )
})


testthat::test_that("question-routing examples expose expected objective metadata", {
  examples = listWMFMEvaluationExamples(package = "WMFM")
  selected = examples[examples$suite == "question_routing", , drop = FALSE]

  testthat::expect_equal(nrow(selected), 12L)
  testthat::expect_true(all(nzchar(selected$expectedArchetype)))
  testthat::expect_true(all(nzchar(selected$expectedRoute)))
  testthat::expect_true(is.logical(selected$expectedFollowup))
})


testthat::test_that("comparison evaluation uses the successful comparison payload", {
  result = list(
    diagnostics = list(
      researchQuestionObjective = list(
        archetype = "compare_groups_or_profiles",
        predictionPayload = list(
          predictionResult = list(status = "needs_input", missingPredictors = "Test")
        ),
        answerPayload = list(
          status = "ok",
          leftProfile = list(Attend = "Yes", Test = 15),
          rightProfile = list(Attend = "No", Test = 15)
        )
      )
    )
  )

  details = getWMFMEvaluationPredictionDetails(result)

  testthat::expect_identical(details$status, "ok")
  testthat::expect_length(details$missing, 0L)
  testthat::expect_equal(details$profile$leftProfile$Test, 15)
  testthat::expect_equal(details$profile$rightProfile$Test, 15)
})
