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
  testthat::expect_match(out$predictionIntervalUnsupportedReason, "not currently supported", fixed = TRUE)
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
