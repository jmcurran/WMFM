testthat::test_that("follow-up control prompt includes deterministic prediction guidance", {
  payload = list(
    supported = TRUE,
    category = "prediction_request"
  )

  block = buildFollowupExplanationControlPromptBlock(followupPayload = payload)

  testthat::expect_type(block, "character")
  testthat::expect_length(block, 1)
  testthat::expect_match(block, "WMFM deterministic prediction payload", fixed = TRUE)
  testthat::expect_match(block, "separate paragraph after the main research-question answer", fixed = TRUE)
})


testthat::test_that("model explanation prompt carries deterministic follow-up diagnostics markers", {
  data = data.frame(
    y = c(10, 12, 11, 13, 15, 14),
    x = c(1, 2, 3, 4, 5, 6)
  )
  model = stats::lm(y ~ x, data = data)

  payload = list(
    supported = TRUE,
    category = "prediction_request",
    predictionResult = list(
      responseScale = "response",
      estimate = 12.5
    )
  )

  attr(model, "wmfm_research_question") = "How does x relate to y?"
  attr(model, "wmfm_model_followup_payload") = payload
  attr(model, "wmfm_model_followup_question") = "What is the predicted y when x is 3?"

  prompt = lmToExplanationPrompt(model)

  testthat::expect_type(prompt, "character")
  testthat::expect_length(prompt, 1)
  testthat::expect_match(prompt, "WMFM deterministic prediction payload", fixed = TRUE)
  testthat::expect_match(prompt, "separate paragraph after the main research-question answer", fixed = TRUE)
})


testthat::test_that("list and load test Course Follow-Up example through display name", {
  examples = listWMFMExamples(includeTestExamples = TRUE)
  testthat::expect_true("test-Course Follow-Up" %in% examples)

  info = loadExampleSpec("test-Course Follow-Up")
  testthat::expect_true(is.list(info$spec))
  testthat::expect_identical(info$spec$displayName, "test-Course Follow-Up")
  testthat::expect_true(nzchar(info$followupQuestion %||% ""))
})
