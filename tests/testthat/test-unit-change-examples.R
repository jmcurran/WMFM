testthat::test_that("Diamonds example loads with deterministic unit-change follow-up", {
  examples = listWMFMExamples(package = "WMFM")
  testthat::expect_true("Diamonds" %in% examples)

  info = loadExampleSpec("Diamonds", package = "WMFM")
  testthat::expect_identical(info$spec$modelType, "lm")
  testthat::expect_identical(info$spec$formula, "price ~ weight")
  testthat::expect_match(info$followupQuestion, "0.1 carat", fixed = TRUE)

  payload = classifyModelFollowupQuestion(info$followupQuestion)
  testthat::expect_identical(payload$category, "unit_change_request")
  testthat::expect_true(payload$supported)
  testthat::expect_true(payload$requiresDeterministicComputation)
  testthat::expect_equal(payload$unitChangeValues, 0.1)
})

testthat::test_that("Quakes unit-change example loads with deterministic follow-up", {
  examples = listWMFMExamples(package = "WMFM")
  testthat::expect_true("Quakes-unit-change" %in% examples)

  info = loadExampleSpec("Quakes-unit-change", package = "WMFM")
  testthat::expect_identical(info$spec$modelType, "poisson")
  testthat::expect_identical(info$spec$formula, "Freq ~ Magnitude")
  testthat::expect_match(info$followupQuestion, "0.1 magnitude", fixed = TRUE)

  payload = classifyModelFollowupQuestion(info$followupQuestion)
  testthat::expect_identical(payload$category, "unit_change_request")
  testthat::expect_true(payload$supported)
  testthat::expect_true(payload$requiresDeterministicComputation)
  testthat::expect_equal(payload$unitChangeValues, 0.1)

  model = stats::glm(
    stats::as.formula(info$spec$formula),
    data = info$data,
    family = stats::poisson()
  )
  payload = enrichFollowupPayloadWithUnitChange(
    model = model,
    followupPayload = payload
  )

  testthat::expect_identical(payload$unitChangeResult$status, "ok")
  testthat::expect_identical(payload$unitChangeResult$modelType, "glm")
  testthat::expect_identical(payload$unitChangeResult$effectScale, "expected_count_multiplier")
  testthat::expect_identical(payload$unitChangeResult$predictorName, "Magnitude")
  testthat::expect_equal(payload$unitChangeResult$requestedUnitChange, 0.1)
})
