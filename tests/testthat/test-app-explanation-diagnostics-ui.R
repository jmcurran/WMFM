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
    assembledPrompt = "Prompt body"
  )

  ui = buildExplanationPromptDiagnosticsUi(diagnostics = diagnostics)
  html = as.character(ui)

  testthat::expect_match(html, "Explanation prompt diagnostics", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_raw_text", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_classification", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_deterministic_status", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_resolved_values", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_missing_values", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_prediction_payload", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_prompt_excerpt", fixed = TRUE)
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

  testthat::expect_match(html, "Copy these blocks", fixed = TRUE)
})
