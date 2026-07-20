testthat::test_that("Observation-question development examples remain hidden from classroom users", {
  visibleExamples = listWMFMExamples(package = "WMFM")
  developerExamples = listWMFMExamples(
    package = "WMFM",
    includeTestExamples = TRUE
  )

  observationExamples = c(
    "Residual Observations Below Expected",
    "Residual Observations Above Expected",
    "Most Unusual Residual Observations",
    "Comparable Diamond Observations",
    "Conditional Diamond Price Percentile",
    "Unsupported Poisson Residual Inspection"
  )

  testthat::expect_true(all(observationExamples %in% developerExamples))
  testthat::expect_false(any(observationExamples %in% visibleExamples))
})


testthat::test_that("Residual examples classify all ranking directions", {
  exampleExpectations = list(
    "Residual Observations Below Expected" = "lower",
    "Residual Observations Above Expected" = "higher",
    "Most Unusual Residual Observations" = "absolute"
  )

  for (exampleName in names(exampleExpectations)) {
    exampleInfo = loadExampleSpec(exampleName)
    payload = classifyModelFollowupQuestion(exampleInfo$followupQuestion)

    testthat::expect_identical(payload$category, "observation_residual_request")
    testthat::expect_identical(
      payload$observationDirection,
      exampleExpectations[[exampleName]]
    )
  }
})


testthat::test_that("Comparable and conditional examples preserve the boundary", {
  comparableInfo = loadExampleSpec("Comparable Diamond Observations")
  comparablePayload = classifyModelFollowupQuestion(
    comparableInfo$followupQuestion
  )

  testthat::expect_identical(
    comparablePayload$category,
    "comparable_observation_request"
  )
  testthat::expect_true(comparablePayload$requiresDeterministicComputation)

  conditionalInfo = loadExampleSpec("Conditional Diamond Price Percentile")
  conditionalPayload = classifyModelFollowupQuestion(
    conditionalInfo$followupQuestion
  )

  testthat::expect_identical(
    conditionalPayload$category,
    "conditional_quantile_request"
  )
  testthat::expect_false(conditionalPayload$supported)
  testthat::expect_match(
    conditionalPayload$deterministicResponse,
    "conditional quantiles",
    fixed = TRUE
  )
})


testthat::test_that("Residual example produces deterministic ranked observations", {
  exampleInfo = loadExampleSpec("Residual Observations Below Expected")
  model = stats::lm(
    stats::as.formula(exampleInfo$spec$formula),
    data = exampleInfo$data
  )
  payload = classifyModelFollowupQuestion(exampleInfo$followupQuestion)
  payload = enrichFollowupPayloadWithObservationResiduals(
    model = model,
    followupPayload = payload
  )

  result = payload$observationResidualResult
  testthat::expect_identical(result$status, "ok")
  testthat::expect_identical(result$direction, "lower")
  testthat::expect_equal(result$observations$residual, sort(result$observations$residual))
  testthat::expect_true(nrow(result$observations) > 0)
})


testthat::test_that("Comparable example resolves fitted-model predictors", {
  exampleInfo = loadExampleSpec("Comparable Diamond Observations")
  model = stats::lm(
    stats::as.formula(exampleInfo$spec$formula),
    data = exampleInfo$data
  )
  payload = classifyModelFollowupQuestion(exampleInfo$followupQuestion)
  payload = enrichFollowupPayloadWithComparableObservations(
    model = model,
    followupPayload = payload,
    neighbourCount = 5L
  )

  result = payload$comparableObservationResult
  testthat::expect_identical(result$status, "ok")
  testthat::expect_equal(as.numeric(result$resolvedPredictorValues$carat), 1)
  testthat::expect_identical(result$resolvedPredictorValues$cut, "Ideal")
  testthat::expect_identical(result$resolvedPredictorValues$color, "G")
  testthat::expect_identical(result$resolvedPredictorValues$clarity, "VS1")
  testthat::expect_identical(nrow(result$observations), 5L)
})


testthat::test_that("Unsupported-model example fails deterministically", {
  exampleInfo = loadExampleSpec("Unsupported Poisson Residual Inspection")
  model = stats::glm(
    stats::as.formula(exampleInfo$spec$formula),
    data = exampleInfo$data,
    family = stats::poisson()
  )
  payload = classifyModelFollowupQuestion(exampleInfo$followupQuestion)
  payload = enrichFollowupPayloadWithObservationResiduals(
    model = model,
    followupPayload = payload
  )

  result = payload$observationResidualResult
  testthat::expect_identical(payload$category, "observation_residual_request")
  testthat::expect_identical(result$status, "unsupported")
  testthat::expect_identical(result$reason, "ordinary_lm_required")
})
