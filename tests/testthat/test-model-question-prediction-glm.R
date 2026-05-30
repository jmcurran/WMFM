testthat::test_that("logistic mean prediction matches stats predict response", {
  df = data.frame(Y = c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 2, 5))
  model = stats::glm(Y ~ X, data = df, family = stats::binomial())

  out = computeGlmModelQuestionPrediction(model, "Predict Y when X = 3")
  ref = as.numeric(stats::predict(model, newdata = data.frame(X = 3), type = "response"))[1]

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$modelType, "glm")
  testthat::expect_identical(out$glmFamily, "binomial")
  testthat::expect_identical(out$glmLink, "logit")
  testthat::expect_identical(out$responseDescription, "probability")
  testthat::expect_identical(out$predictionType, "mean_response_prediction")
  testthat::expect_equal(out$fittedPrediction, ref)
})

testthat::test_that("poisson mean prediction matches stats predict response", {
  df = data.frame(Y = c(1, 2, 4, 2, 3, 5, 6, 4), X = c(1, 2, 3, 4, 2, 5, 6, 3))
  model = stats::glm(Y ~ X, data = df, family = stats::poisson())

  out = computeGlmModelQuestionPrediction(model, "Predict Y when X = 3")
  ref = as.numeric(stats::predict(model, newdata = data.frame(X = 3), type = "response"))[1]

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$glmFamily, "poisson")
  testthat::expect_identical(out$glmLink, "log")
  testthat::expect_identical(out$responseDescription, "expected_count")
  testthat::expect_equal(out$fittedPrediction, ref)
})

testthat::test_that("GLM prediction with missing covariates can fail safely when completion is disabled", {
  df = data.frame(Y = c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 2, 5), G = factor(c("A", "A", "B", "B", "A", "B", "A", "B")))
  model = stats::glm(Y ~ X + G, data = df, family = stats::binomial())

  out = computeGlmModelQuestionPrediction(model, "Predict Y when X = 3", allowMissingPredictorCompletion = FALSE)
  testthat::expect_identical(out$status, "needs_input")
  testthat::expect_identical(out$reason, "missing_predictor_values")
})

testthat::test_that("GLM prediction completes omitted predictors deterministically when allowed", {
  df = data.frame(Y = c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 2, 5), G = factor(c("A", "A", "B", "B", "A", "B", "A", "B")))
  model = stats::glm(Y ~ X + G, data = df, family = stats::binomial())

  out = computeGlmModelQuestionPrediction(model, "Predict Y when X = 3")
  ref = as.numeric(stats::predict(model, newdata = data.frame(X = 3, G = factor("A", levels = levels(df$G))), type = "response"))[1]

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$resolvedPredictorValues$G, "A")
  testthat::expect_equal(out$fittedPrediction, ref)
  testthat::expect_match(paste(out$warnings, collapse = " "), "reference level 'A'", fixed = TRUE)
})

testthat::test_that("GLM factor-level resolution is literal and response-scale", {
  df = data.frame(
    Y = c(0, 1, 0, 1, 1, 0, 1, 0),
    X = c(1, 2, 3, 4, 5, 6, 2, 5),
    G = factor(c("A+B", "yes/no", "A+B", "yes/no", "A+B", "yes/no", "A+B", "yes/no"))
  )
  model = stats::glm(Y ~ X + G, data = df, family = stats::binomial())

  out = computeGlmModelQuestionPrediction(model, "Predict Y when X = 3 and G = yes/no")
  ref = as.numeric(stats::predict(model, newdata = data.frame(X = 3, G = factor("yes/no", levels = levels(df$G))), type = "response"))[1]

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$resolvedPredictorValues$G, "yes/no")
  testthat::expect_equal(out$fittedPrediction, ref)
})

testthat::test_that("GLM confidence interval requests return deterministic response-scale intervals", {
  df = data.frame(Y = c(1, 2, 4, 2, 3, 5, 6, 4), X = c(1, 2, 3, 4, 2, 5, 6, 3))
  model = stats::glm(Y ~ X, data = df, family = stats::poisson())

  out = computeGlmModelQuestionPrediction(model, "Predict Y when X = 3 with confidence interval")
  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$glmFamily, "poisson")
  testthat::expect_true(is.list(out$confidenceInterval))
  testthat::expect_identical(out$confidenceInterval$intervalScale, "response")
  testthat::expect_true(is.list(out$predictionIntervalPolicy))
  testthat::expect_true(out$predictionIntervalPolicy$supported)
  testthat::expect_null(out$predictionInterval)
})

testthat::test_that("GLM deterministic payload is carried in prompt, diagnostics JSON, and appended answer", {
  df = data.frame(Y = c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 2, 5))
  model = stats::glm(Y ~ X, data = df, family = stats::binomial())
  followup = "Predict Y when X = 3"
  payload = classifyModelFollowupQuestion(followupQuestion = followup)
  payload = enrichFollowupPayloadWithLmPrediction(model = model, followupPayload = payload)

  testthat::expect_identical(payload$predictionResult$status, "ok")
  testthat::expect_identical(payload$predictionResult$modelType, "glm")

  block = buildModelFollowupPromptBlock(payload)
  testthat::expect_match(block, "WMFM deterministic GLM follow-up block", fixed = TRUE)
  testthat::expect_match(block, "GLM family: binomial", fixed = TRUE)

  diagnosticsJson = buildExplanationPromptDiagnosticsJson(list(
    followupPayload = payload,
    assembledPrompt = block
  ))
  diagnostics = jsonlite::fromJSON(diagnosticsJson, simplifyVector = FALSE)
  testthat::expect_identical(diagnostics$modelType, "glm")
  testthat::expect_identical(diagnostics$glmFamily, "binomial")
  testthat::expect_identical(diagnostics$glmLink, "logit")
  testthat::expect_identical(diagnostics$predictionPayload$modelType, "glm")
  testthat::expect_match(diagnostics$promptExcerpt, "WMFM deterministic GLM follow-up block", fixed = TRUE)

  attr(model, "wmfm_model_followup_payload") = payload
  answer = appendDeterministicFollowupAnswer("Main answer.", model)
  testthat::expect_match(answer, "For the follow-up question", fixed = TRUE)
  testthat::expect_match(answer, "probability for Y", fixed = TRUE)
})

testthat::test_that("binomial GLM odds requests return deterministic odds-scale intervals", {
  df = data.frame(Y = c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 2, 5))
  model = stats::glm(Y ~ X, data = df, family = stats::binomial())

  out = computeGlmModelQuestionPrediction(model, "What odds for Y would you predict when X = 3 with confidence interval?")
  ref = exp(as.numeric(stats::predict(model, newdata = data.frame(X = 3), type = "link"))[1])

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$glmFamily, "binomial")
  testthat::expect_identical(out$responseScale, "odds")
  testthat::expect_identical(out$responseDescription, "odds")
  testthat::expect_equal(out$fittedPrediction, ref)
  testthat::expect_true(is.list(out$confidenceInterval))
  testthat::expect_identical(out$confidenceInterval$intervalScale, "odds")
  testthat::expect_identical(out$confidenceInterval$method, "link_scale_exponentiate")
})

testthat::test_that("GLM prediction records in-range numeric extrapolation diagnostics", {
  df = data.frame(Y = c(1, 2, 4, 2, 3, 5, 6, 4), X = c(1, 2, 3, 4, 2, 5, 6, 3))
  model = stats::glm(Y ~ X, data = df, family = stats::poisson())

  out = computeGlmModelQuestionPrediction(model, "Predict Y when X = 3")

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$extrapolationPolicy$status, "in_range")
  testthat::expect_equal(out$extrapolationPolicy$numericPredictors$X$observedMin, 1)
  testthat::expect_equal(out$extrapolationPolicy$numericPredictors$X$observedMax, 6)
  testthat::expect_equal(out$extrapolationPolicy$numericPredictors$X$requestedValue, 3)
})

testthat::test_that("GLM prediction warns for slight numeric extrapolation", {
  df = data.frame(Y = c(1, 2, 4, 2, 3, 5, 6, 4), X = c(1, 2, 3, 4, 2, 5, 6, 3))
  model = stats::glm(Y ~ X, data = df, family = stats::poisson())

  out = computeGlmModelQuestionPrediction(model, "Predict Y when X = 6.4")

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$extrapolationPolicy$status, "extrapolation_warning")
  testthat::expect_identical(out$extrapolationPolicy$numericPredictors$X$classification, "extrapolation_warning")
  testthat::expect_match(paste(out$warnings, collapse = " "), "slight extrapolation", fixed = TRUE)
})

testthat::test_that("GLM prediction is suppressed for extreme numeric extrapolation", {
  df = data.frame(Y = c(1, 2, 4, 2, 3, 5, 6, 4), X = c(1, 2, 3, 4, 2, 5, 6, 3))
  model = stats::glm(Y ~ X, data = df, family = stats::poisson())

  out = computeGlmModelQuestionPrediction(model, "Predict Y when X = 8")

  testthat::expect_identical(out$status, "extrapolation_blocked")
  testthat::expect_identical(out$reason, "extrapolation_blocked")
  testthat::expect_identical(out$extrapolationPolicy$status, "extrapolation_blocked")
  testthat::expect_identical(out$extrapolationPolicy$numericPredictors$X$classification, "extrapolation_blocked")
  testthat::expect_null(out$fittedPrediction)
})

testthat::test_that("GLM extrapolation diagnostics expose stable machine-readable fields", {
  df = data.frame(Y = c(1, 2, 4, 2, 3, 5, 6, 4), X = c(1, 2, 3, 4, 2, 5, 6, 3))
  model = stats::glm(Y ~ X, data = df, family = stats::poisson())

  out = computeGlmModelQuestionPrediction(model, "Predict Y when X = 6.4")

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$extrapolationDiagnostics$status, "extrapolation_warning")
  testthat::expect_equal(out$extrapolationDiagnostics$observedRanges$X$lower, 1)
  testthat::expect_equal(out$extrapolationDiagnostics$observedRanges$X$upper, 6)
  testthat::expect_equal(out$extrapolationDiagnostics$requestedValues$X, 6.4)
  testthat::expect_identical(out$extrapolationDiagnostics$classifications$X, "extrapolation_warning")
  testthat::expect_match(out$extrapolationDiagnostics$numericPredictors$X$explanatoryText, "configured tolerance", fixed = TRUE)
  testthat::expect_match(out$extrapolationExplanation, "slight extrapolation", fixed = TRUE)
})

testthat::test_that("blocked GLM extrapolation diagnostics are retained without fitted prediction", {
  df = data.frame(Y = c(1, 2, 4, 2, 3, 5, 6, 4), X = c(1, 2, 3, 4, 2, 5, 6, 3))
  model = stats::glm(Y ~ X, data = df, family = stats::poisson())

  out = computeGlmModelQuestionPrediction(model, "Predict Y when X = 8")

  testthat::expect_identical(out$status, "extrapolation_blocked")
  testthat::expect_identical(out$extrapolationDiagnostics$status, "extrapolation_blocked")
  testthat::expect_equal(out$extrapolationDiagnostics$requestedValues$X, 8)
  testthat::expect_identical(out$extrapolationDiagnostics$classifications$X, "extrapolation_blocked")
  testthat::expect_match(out$extrapolationDiagnostics$numericPredictors$X$explanatoryText, "suppresses the deterministic prediction", fixed = TRUE)
  testthat::expect_match(out$extrapolationExplanation, "suppressed", fixed = TRUE)
  testthat::expect_null(out$fittedPrediction)
})

testthat::test_that("GLM payload records prediction-interval policy metadata", {
  poissonDf = data.frame(Y = c(1, 2, 4, 2, 3, 5, 6, 4), X = c(1, 2, 3, 4, 2, 5, 6, 3))
  poissonModel = stats::glm(Y ~ X, data = poissonDf, family = stats::poisson())

  poissonOut = computeGlmModelQuestionPrediction(poissonModel, "Predict Y when X = 3")

  testthat::expect_identical(poissonOut$status, "ok")
  testthat::expect_true(is.list(poissonOut$predictionIntervalPolicy))
  testthat::expect_true(poissonOut$predictionIntervalPolicy$supported)
  testthat::expect_identical(poissonOut$predictionIntervalPolicy$futureObservationType, "future_count")
  testthat::expect_identical(
    poissonOut$predictionIntervalPolicy$recommendedNextStage,
    "consider_parameter_uncertainty_for_poisson_future_count_interval"
  )
  testthat::expect_identical(poissonOut$predictionIntervalPolicy$method, "conditional_poisson_quantile")
  testthat::expect_null(poissonOut$predictionIntervalUnsupportedReason)

  binomialDf = data.frame(Y = c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 2, 5))
  binomialModel = stats::glm(Y ~ X, data = binomialDf, family = stats::binomial())

  binomialOut = computeGlmModelQuestionPrediction(binomialModel, "Predict Y when X = 3")

  testthat::expect_identical(binomialOut$status, "ok")
  testthat::expect_true(is.list(binomialOut$predictionIntervalPolicy))
  testthat::expect_true(binomialOut$predictionIntervalPolicy$supported)
  testthat::expect_identical(binomialOut$predictionIntervalPolicy$futureObservationType, "future_binary_outcome")
  testthat::expect_identical(
    binomialOut$predictionIntervalPolicy$recommendedNextStage,
    "consider_parameter_uncertainty_for_logistic_probability_interval"
  )
  testthat::expect_identical(binomialOut$predictionIntervalPolicy$method, "bernoulli_outcome_framing")
  testthat::expect_null(binomialOut$predictionIntervalUnsupportedReason)
})

testthat::test_that("explicit Poisson prediction-interval requests return conditional count intervals", {
  df = data.frame(Y = c(1, 2, 4, 2, 3, 5, 6, 4), X = c(1, 2, 3, 4, 2, 5, 6, 3))
  model = stats::glm(Y ~ X, data = df, family = stats::poisson())

  out = computeGlmModelQuestionPrediction(model, "What is the prediction interval when X = 3?")

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$predictionType, "mean_response_prediction")
  testthat::expect_true(is.list(out$predictionIntervalPolicy))
  testthat::expect_true(out$predictionIntervalPolicy$supported)
  testthat::expect_true(out$predictionIntervalPolicy$requested)
  testthat::expect_identical(out$predictionIntervalPolicy$futureObservationType, "future_count")
  testthat::expect_true(is.list(out$predictionInterval))
  testthat::expect_identical(out$predictionInterval$method, "conditional_poisson_quantile")
  testthat::expect_identical(out$predictionInterval$intervalScale, "count")
  testthat::expect_false(out$predictionInterval$parameterUncertaintyIncluded)
  testthat::expect_equal(out$predictionInterval$lwr, stats::qpois(0.025, out$fittedPrediction))
  testthat::expect_equal(out$predictionInterval$upr, stats::qpois(0.975, out$fittedPrediction))
})

testthat::test_that("explicit binomial prediction-interval requests return Bernoulli outcome framing", {
  df = data.frame(Y = c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 2, 5))
  model = stats::glm(Y ~ X, data = df, family = stats::binomial())

  out = computeGlmModelQuestionPrediction(model, "What is the prediction interval when X = 3?")

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$predictionType, "mean_response_prediction")
  testthat::expect_true(is.list(out$predictionIntervalPolicy))
  testthat::expect_true(out$predictionIntervalPolicy$supported)
  testthat::expect_true(out$predictionIntervalPolicy$requested)
  testthat::expect_identical(out$predictionIntervalPolicy$futureObservationType, "future_binary_outcome")
  testthat::expect_true(is.list(out$predictionInterval))
  testthat::expect_identical(out$predictionInterval$method, "bernoulli_outcome_framing")
  testthat::expect_identical(out$predictionInterval$distribution, "bernoulli")
  testthat::expect_false(out$predictionInterval$parameterUncertaintyIncluded)
  testthat::expect_equal(out$predictionInterval$outcomeProbabilities[["1"]], out$fittedPrediction)
  testthat::expect_equal(out$predictionInterval$outcomeProbabilities[["0"]], 1 - out$fittedPrediction)
  testthat::expect_null(out$predictionIntervalUnsupportedReason)
})
