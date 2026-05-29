testthat::test_that("Developer Mode diagnostics UI includes required output IDs", {
  diagnostics = list(
    followupText = "raw text",
    followupPayload = list(
      originalText = "raw text",
      category = "prediction_request",
      predictionResult = list(
        status = "ok",
        resolvedPredictorValues = list(Test = 10, Attend = "regular"),
        warnings = ""
      )
    ),
    assembledPrompt = "Prompt body",
    finalExplanationText = "Final generated explanation."
  )

  ui = buildExplanationPromptDiagnosticsUi(diagnostics = diagnostics)
  html = as.character(ui)

  testthat::expect_match(html, "Explanation prompt diagnostics", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_bundle", fixed = TRUE)
  testthat::expect_match(html, "Raw follow-up question", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_raw_text", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_classification", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_deterministic_status", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_resolved_values", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_missing_values", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_prediction_payload", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_final_explanation_text", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_prompt_excerpt", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_json_download", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_json_bundle", fixed = TRUE)
  testthat::expect_match(html, "wmfm-followup-diagnostics-YYYYMMDD-HHMMSS.json", fixed = TRUE)
})


testthat::test_that("diagnostics helper tells developers what to copy", {
  diagnostics = list(
    followupText = "raw text",
    followupPayload = list(
      originalText = "raw text",
      category = "prediction_request",
      predictionResult = list(status = "ok")
    ),
    assembledPrompt = "Prompt body"
  )

  html = as.character(buildExplanationPromptDiagnosticsUi(diagnostics = diagnostics))

  testthat::expect_match(html, "Copy the diagnostics bundle", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_bundle", fixed = TRUE)
})


testthat::test_that("diagnostics JSON helper exposes uploadable follow-up fields", {
  diagnostics = list(
    followupText = "raw text",
    followupPayload = list(
      originalText = "raw text",
      category = "prediction_request",
      predictionResult = list(
        status = "ok",
        suppliedPredictorValues = list(Test = "10"),
        resolvedPredictorValues = list(Test = 10, Attend = "Yes"),
        completedPredictorValues = list(Test = 10, Attend = "Yes")
      )
    ),
    assembledPrompt = "Prompt body",
    finalExplanationText = "Final generated explanation."
  )

  out = buildExplanationPromptDiagnosticsJson(diagnostics = diagnostics)

  testthat::expect_match(out, "rawFollowupQuestion", fixed = TRUE)
  testthat::expect_match(out, "completedPredictorValues", fixed = TRUE)
  testthat::expect_match(out, '"Attend": "Yes"', fixed = TRUE)
  testthat::expect_match(out, "assembledPromptExcerpt", fixed = TRUE)
  testthat::expect_match(out, "finalExplanationText", fixed = TRUE)
  testthat::expect_match(out, "Final generated explanation.", fixed = TRUE)
})

testthat::test_that("diagnostics JSON download uses Shiny download output", {
  diagnostics = list(
    followupText = "raw text",
    followupPayload = list(
      originalText = "raw text",
      category = "prediction_request",
      predictionResult = list(status = "ok")
    ),
    assembledPrompt = "Prompt body"
  )

  html = as.character(buildExplanationPromptDiagnosticsUi(diagnostics = diagnostics))

  testthat::expect_match(html, "Download diagnostics JSON", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_json_download", fixed = TRUE)
  testthat::expect_no_match(html, "URL.createObjectURL", fixed = TRUE)
})
