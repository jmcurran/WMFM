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
  testthat::expect_match(html, "diag_followup_bundle", fixed = TRUE)
  testthat::expect_match(html, "Raw follow-up question", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_raw_text", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_classification", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_deterministic_status", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_resolved_values", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_missing_values", fixed = TRUE)
  testthat::expect_match(html, "diag_followup_prediction_payload", fixed = TRUE)
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
    assembledPrompt = "Prompt body"
  )

  out = buildExplanationPromptDiagnosticsJson(diagnostics = diagnostics)

  testthat::expect_match(out, "rawFollowupQuestion", fixed = TRUE)
  testthat::expect_match(out, "completedPredictorValues", fixed = TRUE)
  testthat::expect_match(out, '"Attend": "Yes"', fixed = TRUE)
  testthat::expect_match(out, "assembledPromptExcerpt", fixed = TRUE)
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

testthat::test_that("diagnostics JSON sanitises ellmer output explanation objects", {
  explanationObject = list(content = "Generated Poisson explanation", meta = list(tokens = 12))
  class(explanationObject) = c("ellmer_output", "list")

  diagnostics = list(
    followupText = "raw text",
    followupPayload = list(
      originalText = "raw text",
      category = "prediction_request",
      predictionResult = list(
        status = "ok",
        modelType = "glm",
        glmFamily = "poisson",
        glmLink = "log",
        responseScale = "response",
        responseDescription = "expected_count",
        confidenceInterval = list(
          fit = 3.2,
          lwr = 2.1,
          upr = 4.7,
          intervalScale = "response"
        ),
        predictionIntervalUnsupportedReason = "not currently supported"
      )
    ),
    generatedExplanation = explanationObject,
    assembledPrompt = "Prompt body"
  )

  out = buildExplanationPromptDiagnosticsJson(diagnostics = diagnostics)
  parsed = jsonlite::fromJSON(out, simplifyVector = FALSE)

  testthat::expect_identical(parsed$generatedExplanation, "Generated Poisson explanation")
  testthat::expect_identical(parsed$predictionPayload$glmFamily, "poisson")
  testthat::expect_identical(parsed$predictionPayload$confidenceInterval$intervalScale, "response")
  testthat::expect_match(
    parsed$predictionPayload$predictionIntervalUnsupportedReason,
    "not currently supported",
    fixed = TRUE
  )
})


testthat::test_that("diagnostics UI and JSON expose unit-change payload", {
  diagnostics = list(
    followupText = "Explain the Carat effect for a 0.1-unit increase",
    followupPayload = list(
      originalText = "Explain the Carat effect for a 0.1-unit increase",
      category = "alternative_unit_change",
      unitChangeResult = list(
        status = "ok",
        modelType = "lm",
        predictorName = "Carat",
        requestedUnitChange = 0.1,
        unitChangeEffect = 123.4,
        confidenceInterval = list(lwr = 100, upr = 150)
      )
    ),
    assembledPrompt = "WMFM deterministic requested unit-change interpretation"
  )

  html = paste(capture.output(print(buildExplanationPromptDiagnosticsUi(diagnostics))), collapse = "\n")
  json = buildExplanationPromptDiagnosticsJson(diagnostics)

  testthat::expect_match(html, "diag_followup_unit_change_payload", fixed = TRUE)
  testthat::expect_match(html, "deterministic prediction or unit-change payload", fixed = TRUE)
  testthat::expect_match(json, "unitChangePayload", fixed = TRUE)
  testthat::expect_match(json, "requestedUnitChange", fixed = TRUE)
  testthat::expect_match(json, "transformedUnitChangeEffect", fixed = TRUE)
})
