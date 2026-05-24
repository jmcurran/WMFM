testthat::test_that("lm prediction with numeric predictor matches stats predict", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Exam ~ Test, data = df)

  out = computeLmModelQuestionPrediction(model, "Predict exam when Test = 10")
  ref = as.numeric(stats::predict(model, newdata = data.frame(Test = 10)))[1]

  testthat::expect_identical(out$status, "ok")
  testthat::expect_equal(out$fittedPrediction, ref)
})

testthat::test_that("lm prediction confidence interval matches stats predict confidence interval", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Exam ~ Test, data = df)

  out = computeLmModelQuestionPrediction(model, "Predict exam at Test = 10 with confidence interval")
  ref = stats::predict(model, newdata = data.frame(Test = 10), interval = "confidence")

  testthat::expect_equal(out$confidenceInterval$fit, as.numeric(ref[1, "fit"]))
  testthat::expect_equal(out$confidenceInterval$lwr, as.numeric(ref[1, "lwr"]))
  testthat::expect_equal(out$confidenceInterval$upr, as.numeric(ref[1, "upr"]))
})



testthat::test_that("lm prediction interval matches stats predict prediction interval", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Exam ~ Test, data = df)

  out = computeLmModelQuestionPrediction(model, "Give a prediction interval when Test = 10")
  ref = stats::predict(model, newdata = data.frame(Test = 10), interval = "prediction")

  testthat::expect_identical(out$predictionType, "individual_prediction_interval")
  testthat::expect_equal(out$predictionInterval$fit, as.numeric(ref[1, "fit"]))
  testthat::expect_equal(out$predictionInterval$lwr, as.numeric(ref[1, "lwr"]))
  testthat::expect_equal(out$predictionInterval$upr, as.numeric(ref[1, "upr"]))
})



testthat::test_that("plural prediction intervals wording is recognized", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Exam ~ Test, data = df)

  out = computeLmModelQuestionPrediction(model, "Show prediction intervals when Test = 10")

  testthat::expect_identical(out$predictionType, "individual_prediction_interval")
  testthat::expect_true(!is.null(out$predictionInterval))
})

testthat::test_that("prediction interval is wider than confidence interval", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Exam ~ Test, data = df)
  ci = stats::predict(model, newdata = data.frame(Test = 10), interval = "confidence")
  pi = stats::predict(model, newdata = data.frame(Test = 10), interval = "prediction")

  ciWidth = as.numeric(ci[1, "upr"] - ci[1, "lwr"])
  piWidth = as.numeric(pi[1, "upr"] - pi[1, "lwr"])
  testthat::expect_gt(piWidth, ciWidth)
})
testthat::test_that("factor predictor values are validated against known levels", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Attend = factor(c("No", "Yes", "Yes", "Yes", "No", "Yes")))
  model = stats::lm(Exam ~ Attend, data = df)

  out = computeLmModelQuestionPrediction(model, "Predict exam for Attend = Maybe")
  testthat::expect_identical(out$status, "needs_input")
  testthat::expect_identical(out$reason, "invalid_factor_level")
})

testthat::test_that("missing required predictor values fail safely", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7), Attend = factor(c("No", "Yes", "Yes", "Yes", "No", "Yes")))
  model = stats::lm(Exam ~ Test + Attend, data = df)

  out = computeLmModelQuestionPrediction(model, "Predict exam for Test = 10")
  testthat::expect_identical(out$status, "needs_input")
  testthat::expect_identical(out$reason, "missing_predictor_values")
})

testthat::test_that("unsupported model types fail safely", {
  df = data.frame(Y = c(0, 1, 0, 1, 1, 0), X = c(1, 2, 3, 4, 5, 6))
  model = stats::glm(Y ~ X, data = df, family = stats::binomial())
  out = computeLmModelQuestionPrediction(model, "Predict Y when X = 3")

  testthat::expect_identical(out$status, "unsupported")
})

testthat::test_that("prompt includes deterministic prediction payload values when available", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Exam ~ Test, data = df)
  payload = classifyModelFollowupQuestion("Give a prediction interval when Test = 10")
  payload = enrichFollowupPayloadWithLmPrediction(model, payload)
  attr(model, "wmfm_model_followup_payload") = payload

  prompt = lmToExplanationPrompt(model)
  testthat::expect_match(prompt, "WMFM deterministic prediction payload", fixed = TRUE)
  testthat::expect_match(prompt, "computed deterministically by WMFM", fixed = TRUE)
  testthat::expect_match(prompt, "Prediction interval for an individual outcome", fixed = TRUE)
  testthat::expect_match(prompt, "Do not call a confidence interval for the average/expected response a prediction interval", fixed = TRUE)
})

testthat::test_that("prompt payload forbids recomputation or invented intervals", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Exam ~ Test, data = df)
  payload = classifyModelFollowupQuestion("Predict exam when Test = 10")
  payload = enrichFollowupPayloadWithLmPrediction(model, payload)
  attr(model, "wmfm_model_followup_payload") = payload

  prompt = lmToExplanationPrompt(model)
  testthat::expect_match(prompt, "Do not recompute, round further, or invent intervals", fixed = TRUE)
  testthat::expect_match(prompt, "Do not invent prediction intervals", fixed = TRUE)
})

testthat::test_that("no prediction payload is added for empty follow-up text", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_model_followup_question") = ""

  prompt = lmToExplanationPrompt(model)
  testthat::expect_no_match(prompt, "WMFM deterministic prediction payload", fixed = TRUE)
})


testthat::test_that("non-numeric values for numeric predictors fail safely", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Exam ~ Test, data = df)

  out = computeLmModelQuestionPrediction(model, "Predict exam when Test = abc")
  testthat::expect_identical(out$status, "needs_input")
  testthat::expect_identical(out$reason, "invalid_numeric_value")
})

testthat::test_that("unsupported predictor types fail safely", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), IsRemote = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
  model = stats::lm(Exam ~ IsRemote, data = df)

  out = computeLmModelQuestionPrediction(model, "Predict exam when IsRemote = TRUE")
  testthat::expect_identical(out$status, "needs_input")
  testthat::expect_identical(out$reason, "unsupported_predictor_type")
})


testthat::test_that("unsupported GLM prediction interval requests fail safely", {
  df = data.frame(Y = c(0, 1, 0, 1, 1, 0), X = c(1, 2, 3, 4, 5, 6))
  model = stats::glm(Y ~ X, data = df, family = stats::binomial())
  out = computeLmModelQuestionPrediction(model, "Give a prediction interval when X = 3")

  testthat::expect_identical(out$status, "unsupported")
})


testthat::test_that("unsupported model reason code is normalized", {
  df = data.frame(Y = c(0, 1, 0, 1, 1, 0), X = c(1, 2, 3, 4, 5, 6))
  model = stats::glm(Y ~ X, data = df, family = stats::binomial())
  out = computeLmModelQuestionPrediction(model, "Predict Y when X = 3")

  testthat::expect_identical(out$reason, "unsupported_model_type")
  testthat::expect_identical(out$reasonDetail, "stage23.7_supports_only_ordinary_lm_prediction_intervals")
  testthat::expect_match(out$warnings, "currently supports ordinary linear-model prediction follow-ups only", fixed = TRUE)
})

testthat::test_that("unsupported separator x:5 fails safely as missing predictor values", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Exam ~ Test, data = df)

  out = computeLmModelQuestionPrediction(model, "predict something around x:5")
  testthat::expect_identical(out$status, "needs_input")
  testthat::expect_identical(out$reason, "missing_predictor_values")
})
