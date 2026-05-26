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
