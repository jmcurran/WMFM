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

testthat::test_that("factor predictor values are validated against known levels", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Attend = factor(c("No", "Yes", "Yes", "Yes", "No", "Yes")))
  model = stats::lm(Exam ~ Attend, data = df)

  testthat::expect_error(
    computeLmModelQuestionPrediction(model, "Predict exam for Attend = Maybe"),
    "Unsupported factor level"
  )
})

testthat::test_that("missing required predictor values fail safely", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7), Attend = factor(c("No", "Yes", "Yes", "Yes", "No", "Yes")))
  model = stats::lm(Exam ~ Test + Attend, data = df)

  out = computeLmModelQuestionPrediction(model, "Predict exam for Test = 10")
  testthat::expect_identical(out$status, "needs_input")
  testthat::expect_identical(out$reason, "missing_required_predictor_values")
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
  payload = classifyModelFollowupQuestion("Predict exam when Test = 10")
  payload = enrichFollowupPayloadWithLmPrediction(model, payload)
  attr(model, "wmfm_model_followup_payload") = payload

  prompt = lmToExplanationPrompt(model)
  testthat::expect_match(prompt, "WMFM deterministic prediction payload", fixed = TRUE)
  testthat::expect_match(prompt, "computed deterministically by WMFM", fixed = TRUE)
})

testthat::test_that("prompt payload forbids recomputation or invented intervals", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Exam ~ Test, data = df)
  payload = classifyModelFollowupQuestion("Predict exam when Test = 10")
  payload = enrichFollowupPayloadWithLmPrediction(model, payload)
  attr(model, "wmfm_model_followup_payload") = payload

  prompt = lmToExplanationPrompt(model)
  testthat::expect_match(prompt, "Do not recompute, round further, or invent intervals", fixed = TRUE)
  testthat::expect_match(prompt, "Do not call this an individual prediction interval", fixed = TRUE)
})

testthat::test_that("no prediction payload is added for empty follow-up text", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_model_followup_question") = ""

  prompt = lmToExplanationPrompt(model)
  testthat::expect_no_match(prompt, "WMFM deterministic prediction payload", fixed = TRUE)
})
