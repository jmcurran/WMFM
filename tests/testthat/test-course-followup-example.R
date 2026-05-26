testthat::test_that("Course Follow-Up example exists and loads follow-up question", {
  examples = listWMFMExamples(package = "WMFM")
  testthat::expect_true("Course Follow-Up" %in% examples)

  info = loadExampleSpec("Course Follow-Up")
  testthat::expect_match(
    info$researchQuestion,
    "Do students who attend class regularly and score higher on the mid-term test tend to get better final exam marks\\?"
  )
  testthat::expect_match(
    info$followupQuestion,
    "If I score 10 out of 20 on the test and I attend class regularly what is my predicted mark for the final exam\\?"
  )
})


testthat::test_that("follow-up prediction request is classified and deterministic payload reaches prompt", {
  df = data.frame(
    Exam = c(45, 54, 62, 71, 80, 88),
    Test = c(8, 10, 12, 14, 16, 18),
    Attend = factor(c("not", "not", "regular", "regular", "regular", "regular"), levels = c("not", "regular"))
  )
  model = stats::lm(Exam ~ Attend + Test, data = df)
  followup = "If I score 10 out of 20 on the test and I attend class regularly what is my predicted mark for the final exam?"

  payload = classifyModelFollowupQuestion(followupQuestion = followup)
  payload = enrichFollowupPayloadWithLmPrediction(model = model, followupPayload = payload)

  testthat::expect_identical(payload$category, "prediction_request")
  testthat::expect_identical(payload$predictionResult$status, "ok")
  testthat::expect_equal(as.numeric(payload$predictionResult$resolvedPredictorValues$Test), 10)
  testthat::expect_identical(payload$predictionResult$resolvedPredictorValues$Attend, "regular")

  attr(model, "wmfm_research_question") = "Do students who attend class regularly and score higher on the mid-term test tend to get better final exam marks?"
  attr(model, "wmfm_model_followup_question") = followup
  attr(model, "wmfm_model_followup_payload") = payload
  prompt = lmToExplanationPrompt(model)

  testthat::expect_match(prompt, "WMFM deterministic prediction payload", fixed = TRUE)
  testthat::expect_match(prompt, followup, fixed = TRUE)
  testthat::expect_match(prompt, "separate paragraph after the main research-question answer", fixed = TRUE)
  testthat::expect_match(prompt, "Fitted mean prediction:", fixed = TRUE)
  testthat::expect_match(prompt, "Prediction interval for an individual outcome", fixed = TRUE)
  testthat::expect_match(prompt, "Put this follow-up answer in a separate paragraph", fixed = TRUE)
  testthat::expect_match(prompt, "WMFM will append the deterministic numeric follow-up answer", fixed = TRUE)
})

testthat::test_that("Course Follow-Up never defaults regular attendance to not", {
  df = data.frame(
    Exam = c(45, 54, 62, 71, 80, 88),
    Test = c(8, 10, 12, 14, 16, 18),
    Attend = factor(c("not", "not", "regular", "regular", "regular", "regular"), levels = c("not", "regular"))
  )
  model = stats::lm(Exam ~ Attend + Test, data = df)
  followup = "If I score 10 out of 20 on the test and I attend class regularly what is my predicted mark for the final exam?"

  payload = classifyModelFollowupQuestion(followupQuestion = followup)
  payload = enrichFollowupPayloadWithLmPrediction(model = model, followupPayload = payload)

  if (identical(payload$predictionResult$status, "ok")) {
    testthat::expect_identical(payload$predictionResult$resolvedPredictorValues$Attend, "regular")
  } else {
    testthat::expect_true(payload$predictionResult$status %in% c("needs_input", "clarification_required", "unsupported"))
  }
  testthat::expect_false(identical(payload$predictionResult$resolvedPredictorValues$Attend %||% NULL, "not"))
})

testthat::test_that("deterministic follow-up answer is appended after chat output", {
  df = data.frame(
    Exam = c(45, 54, 62, 71, 80, 88),
    Test = c(8, 10, 12, 14, 16, 18),
    Attend = factor(c("not", "not", "regular", "regular", "regular", "regular"), levels = c("not", "regular"))
  )
  model = stats::lm(Exam ~ Attend + Test, data = df)
  followup = "If I score 10 out of 20 on the test and I attend class regularly what is my predicted mark for the final exam?"
  payload = classifyModelFollowupQuestion(followupQuestion = followup)
  payload = enrichFollowupPayloadWithLmPrediction(model = model, followupPayload = payload)
  attr(model, "wmfm_model_followup_payload") = payload

  out = appendDeterministicFollowupAnswer(
    explanation = "Main research-question answer.",
    model = model
  )

  testthat::expect_match(out, "Main research-question answer\\.\\n\\nFor the follow-up question", perl = TRUE)
  testthat::expect_match(out, "Test = 10", fixed = TRUE)
  testthat::expect_match(out, "Attend = regular", fixed = TRUE)
  testthat::expect_match(out, "95% prediction interval", fixed = TRUE)
  testthat::expect_no_match(out, "all other predictors in the dataset", fixed = TRUE)
})

testthat::test_that("deterministic follow-up failure text only asks for fitted-model predictors", {
  prediction = list(
    status = "needs_input",
    reason = "missing_predictor_values",
    warnings = "Missing fitted-model predictor values: Attend."
  )

  out = buildDeterministicFollowupFailureAnswer(prediction = prediction)

  testthat::expect_match(out, "Only predictors used by the fitted model are required", fixed = TRUE)
  testthat::expect_no_match(out, "all other predictors in the dataset", fixed = TRUE)
})

testthat::test_that("deterministic follow-up answer removes unsafe LLM fallback paragraph", {
  df = data.frame(
    Exam = c(45, 54, 62, 71, 80, 88),
    Test = c(8, 10, 12, 14, 16, 18),
    Attend = factor(c("not", "not", "regular", "regular", "regular", "regular"), levels = c("not", "regular"))
  )
  model = stats::lm(Exam ~ Attend + Test, data = df)
  followup = "If I score 10 out of 20 on the test and I attend class regularly what is my predicted mark for the final exam?"
  payload = classifyModelFollowupQuestion(followupQuestion = followup)
  payload = enrichFollowupPayloadWithLmPrediction(model = model, followupPayload = payload)
  attr(model, "wmfm_model_followup_payload") = payload

  explanation = paste(
    "Main research-question answer.",
    "To predict the final exam mark for this student, the model would need the values for all other predictors in the dataset.",
    sep = "\n\n"
  )

  out = appendDeterministicFollowupAnswer(
    explanation = explanation,
    model = model
  )

  testthat::expect_match(out, "Main research-question answer", fixed = TRUE)
  testthat::expect_no_match(out, "all other predictors in the dataset", fixed = TRUE)
  testthat::expect_match(out, "For the follow-up question", fixed = TRUE)
  testthat::expect_match(out, "WMFM predicts", fixed = TRUE)
  testthat::expect_match(out, "95% prediction interval", fixed = TRUE)
})

testthat::test_that("deterministic follow-up answer is not duplicated", {
  df = data.frame(
    Exam = c(45, 54, 62, 71, 80, 88),
    Test = c(8, 10, 12, 14, 16, 18),
    Attend = factor(c("not", "not", "regular", "regular", "regular", "regular"), levels = c("not", "regular"))
  )
  model = stats::lm(Exam ~ Attend + Test, data = df)
  followup = "If I score 10 out of 20 on the test and I attend class regularly what is my predicted mark for the final exam?"
  payload = classifyModelFollowupQuestion(followupQuestion = followup)
  payload = enrichFollowupPayloadWithLmPrediction(model = model, followupPayload = payload)
  attr(model, "wmfm_model_followup_payload") = payload

  once = appendDeterministicFollowupAnswer(
    explanation = "Main research-question answer.",
    model = model
  )
  twice = appendDeterministicFollowupAnswer(
    explanation = once,
    model = model
  )

  testthat::expect_identical(twice, once)
})

testthat::test_that("deterministic follow-up answer removes LLM-authored prediction sentences from same paragraph", {
  df = data.frame(
    Exam = c(45, 54, 62, 71, 80, 88),
    Test = c(8, 10, 12, 14, 16, 18),
    Attend = factor(c("not", "not", "regular", "regular", "regular", "regular"), levels = c("not", "regular"))
  )
  model = stats::lm(Exam ~ Attend + Test, data = df)
  followup = "If I score 10 out of 20 on the test and I attend class regularly what is my predicted mark for the final exam?"
  payload = classifyModelFollowupQuestion(followupQuestion = followup)
  payload = enrichFollowupPayloadWithLmPrediction(model = model, followupPayload = payload)
  payload$predictionResult$fittedPrediction = 49.8773
  payload$predictionResult$predictionInterval = list(
    fit = 49.8773,
    lwr = 26.9374,
    upr = 72.8173,
    level = 0.95
  )
  attr(model, "wmfm_model_followup_payload") = payload

  explanation = paste(
    "Main research-question answer. If a student scores 10 on the mid-term test and attends class regularly, the expected final exam mark is 49.8773. A 95% prediction interval for an individual student's final mark is from 26.9374 to 72.8173.",
    collapse = " "
  )

  out = appendDeterministicFollowupAnswer(
    explanation = explanation,
    model = model
  )

  testthat::expect_match(out, "Main research-question answer", fixed = TRUE)
  testthat::expect_no_match(out, "49.8773", fixed = TRUE)
  testthat::expect_no_match(out, "26.9374", fixed = TRUE)
  testthat::expect_match(out, "\\n\\nFor the follow-up question", perl = TRUE)
  testthat::expect_match(out, "49.88", fixed = TRUE)
  testthat::expect_match(out, "26.94", fixed = TRUE)
})
