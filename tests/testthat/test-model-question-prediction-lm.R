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



testthat::test_that("individual lm prediction wording requests prediction interval", {
  df = data.frame(
    Exam = c(45, 54, 62, 71, 80, 88),
    Test = c(8, 10, 12, 14, 16, 18),
    Attend = factor(c("not", "not", "regular", "regular", "regular", "regular"), levels = c("not", "regular"))
  )
  model = stats::lm(Exam ~ Attend + Test, data = df)

  out = computeLmModelQuestionPrediction(
    model,
    "If I score 10 out of 20 on the test and I attend class regularly what is my predicted mark for the final exam?"
  )

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$predictionType, "individual_prediction_interval")
  testthat::expect_true(is.list(out$predictionInterval))
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

testthat::test_that("omitted fitted-model predictor values are completed deterministically", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7), Attend = factor(c("No", "Yes", "Yes", "Yes", "No", "Yes")))
  model = stats::lm(Exam ~ Test + Attend, data = df)

  out = computeLmModelQuestionPrediction(model, "Predict exam for Test = 10")
  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$resolvedPredictorValues$Attend, "No")
  testthat::expect_match(paste(out$warnings, collapse = " "), "reference level", fixed = TRUE)
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

testthat::test_that("factor prediction resolves explicit coded levels from model factor levels", {
  df = data.frame(
    Response = c(45, 54, 62, 71, 80, 88),
    X = c(8, 10, 12, 14, 16, 18),
    Group = factor(c("baseline", "baseline", "target", "target", "target", "target"), levels = c("baseline", "target"))
  )
  model = stats::lm(Response ~ Group + X, data = df)

  out = computeLmModelQuestionPrediction(
    model,
    "Predict response when X = 10 and Group = target."
  )

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$resolvedPredictorValues$Group, "target")
})

testthat::test_that("unmodelled extra wording does not block explicit prediction inputs", {
  df = data.frame(Response = c(42, 58, 81, 86, 35, 72), X = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Response ~ X, data = df)

  out = computeLmModelQuestionPrediction(model, "include this extra context; predict response when X = 10")
  testthat::expect_identical(out$status, "ok")
})

testthat::test_that("non-Attend factor predictors resolve their own levels", {
  df = data.frame(
    Exam = c(40, 50, 60, 70, 80, 90),
    Test = c(8, 10, 12, 14, 16, 18),
    Group = factor(c("control", "control", "treated", "treated", "treated", "control"), levels = c("control", "treated"))
  )
  model = stats::lm(Exam ~ Test + Group, data = df)

  out = computeLmModelQuestionPrediction(model, "predict exam when test = 10 for treated group")
  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$resolvedPredictorValues$Group, "treated")
})

testthat::test_that("ambiguous factor wording fails safely", {
  df = data.frame(
    Exam = c(40, 50, 60, 70, 80, 90),
    Test = c(8, 10, 12, 14, 16, 18),
    Group = factor(c("yes", "no", "yes", "no", "yes", "no"), levels = c("yes", "no"))
  )
  model = stats::lm(Exam ~ Test + Group, data = df)

  out = computeLmModelQuestionPrediction(model, "predict exam when test = 10; yes and no both seem possible")
  testthat::expect_identical(out$status, "clarification_required")
  testthat::expect_identical(out$reason, "clarification_required")
})

testthat::test_that("factor level resolution handles regex metacharacters safely", {
  df = data.frame(
    Exam = c(40, 45, 50, 55, 60, 65, 70, 75),
    Test = c(8, 9, 10, 11, 12, 13, 14, 15),
    Group = factor(
      c("A+B", "yes/no", "group (1)", "x{2}", "A+B", "yes/no", "group (1)", "x{2}"),
      levels = c("A+B", "yes/no", "group (1)", "x{2}")
    )
  )
  model = stats::lm(Exam ~ Test + Group, data = df)

  out_plus = computeLmModelQuestionPrediction(model, "predict exam when test = 10 and group = A+B")
  testthat::expect_identical(out_plus$status, "ok")
  testthat::expect_identical(out_plus$resolvedPredictorValues$Group, "A+B")

  out_slash = computeLmModelQuestionPrediction(model, "predict exam when test = 10 and group = yes/no")
  testthat::expect_identical(out_slash$status, "ok")
  testthat::expect_identical(out_slash$resolvedPredictorValues$Group, "yes/no")

  out_paren = computeLmModelQuestionPrediction(model, "predict exam when test = 10 for group (1)")
  testthat::expect_identical(out_paren$status, "ok")
  testthat::expect_identical(out_paren$resolvedPredictorValues$Group, "group (1)")

  out_braces = computeLmModelQuestionPrediction(model, "predict exam when test = 10 for x{2}")
  testthat::expect_identical(out_braces$status, "ok")
  testthat::expect_identical(out_braces$resolvedPredictorValues$Group, "x{2}")
})

testthat::test_that("matchFactorLevelInFollowupText preserves internal punctuation", {
  levels = c("A+B", "yes/no", "regular")

  out_plus = matchFactorLevelInFollowupText(
    levels = levels,
    followupText = "Please predict for group = a+b."
  )
  testthat::expect_identical(out_plus, "A+B")

  out_slash = matchFactorLevelInFollowupText(
    levels = levels,
    followupText = "I mean the YES/NO group?"
  )
  testthat::expect_identical(out_slash, "yes/no")

  out_regular = matchFactorLevelInFollowupText(
    levels = levels,
    followupText = "I attend regularly."
  )
  testthat::expect_identical(out_regular, "regular")
})


testthat::test_that("matchFactorLevelsInPredictionText matches punctuated levels in plain follow-up text", {
  levels = c("A+B", "control")
  out_plus = matchFactorLevelsInPredictionText(
    levels = levels,
    text = "predict y when x = 10 and group is A+B"
  )
  testthat::expect_identical(out_plus, "A+B")

  levels2 = c("yes/no", "control")
  out_slash = matchFactorLevelsInPredictionText(
    levels = levels2,
    text = "predict y when x = 10 and group is yes/no"
  )
  testthat::expect_identical(out_slash, "yes/no")
})

testthat::test_that("assignment extraction keeps separate predictor assignments with punctuated factor values", {
  out_plus = extractPredictionAssignmentPairs(
    "predict exam when test = 10 and group = A+B"
  )
  testthat::expect_identical(out_plus$test, "10")
  testthat::expect_identical(out_plus$group, "A+B")

  out_slash = extractPredictionAssignmentPairs(
    "predict exam when test = 10 and group = yes/no"
  )
  testthat::expect_identical(out_slash$test, "10")
  testthat::expect_identical(out_slash$group, "yes/no")
})

testthat::test_that("binary factor prediction uses explicit coded levels", {
  df = data.frame(
    Response = c(45, 54, 62, 71, 80, 88),
    X = c(8, 10, 12, 14, 16, 18),
    Group = factor(c("No", "No", "Yes", "Yes", "Yes", "Yes"), levels = c("No", "Yes"))
  )
  model = stats::lm(Response ~ Group + X, data = df)

  out = computeLmModelQuestionPrediction(
    model,
    "Predict response when X = 10 and Group = Yes."
  )

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$resolvedPredictorValues$Group, "Yes")
  testthat::expect_false(grepl("reference level 'No'", paste(out$warnings, collapse = " "), fixed = TRUE))
})

testthat::test_that("numeric prediction parsing escapes transformed predictor names", {
  valueAfter = extractNaturalNumericPredictionValue(
    predictor = "log(carat)",
    text = "Predict log price when log(carat) = 0.5"
  )
  testthat::expect_identical(valueAfter, "0.5")

  valueBefore = extractNaturalNumericPredictionValue(
    predictor = "log(carat)",
    text = "Predict log price for 0.5 on log(carat)"
  )
  testthat::expect_identical(valueBefore, "0.5")

  valueBeforeWithSentencePunctuation = extractNaturalNumericPredictionValue(
    predictor = "log(carat)",
    text = "Predict log price for 0.5 on log(carat)."
  )
  testthat::expect_identical(valueBeforeWithSentencePunctuation, "0.5")
})
