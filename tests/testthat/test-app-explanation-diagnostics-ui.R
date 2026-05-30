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


testthat::test_that("diagnostics JSON exposes full unit-change debug fields", {
  diagnostics = list(
    followupText = "Explain the Magnitude effect for a 0.1 magnitude increase",
    followupPayload = list(
      originalText = "Explain the Magnitude effect for a 0.1 magnitude increase",
      category = "unit_change_request",
      unitChangeResult = list(
        status = "ok",
        modelType = "glm",
        glmFamily = "poisson",
        glmLink = "log",
        effectScale = "expected_count_multiplier",
        responseName = "Freq",
        predictorName = "Magnitude",
        requestedUnitChange = 0.1,
        oneUnitEffect = -1.56,
        transformedEstimate = 0.8559,
        unitChangeEffect = 0.8559,
        confidenceInterval = list(
          level = 0.95,
          lwr = 0.81,
          upr = 0.91,
          percentChangeLwr = -19,
          percentChangeUpr = -9
        ),
        warnings = ""
      )
    ),
    assembledPrompt = paste(
      "WMFM deterministic requested unit-change interpretation:",
      "Requested unit change: 0.1"
    )
  )

  out = buildExplanationPromptDiagnosticsJson(diagnostics)
  parsed = jsonlite::fromJSON(out, simplifyVector = FALSE)

  testthat::expect_identical(parsed$rawFollowupQuestion, diagnostics$followupText)
  testthat::expect_identical(parsed$followupCategory, "unit_change_request")
  testthat::expect_identical(parsed$modelType, "glm")
  testthat::expect_identical(parsed$glmFamily, "poisson")
  testthat::expect_identical(parsed$glmLink, "log")
  testthat::expect_identical(parsed$effectScale, "expected_count_multiplier")
  testthat::expect_identical(parsed$requestedPredictor, "Magnitude")
  testthat::expect_equal(parsed$requestedUnitChange, 0.1)
  testthat::expect_equal(parsed$originalUnitChangeEffect, -1.56)
  testthat::expect_equal(parsed$transformedUnitChangeEffect, 0.8559)
  testthat::expect_equal(parsed$transformedUnitChangeInterval$lwr, 0.81)
  testthat::expect_match(parsed$promptExcerpt, "Requested unit change: 0.1", fixed = TRUE)
})


testthat::test_that("diagnostics bundle exposes unit-change debug fields", {
  diagnostics = list(
    followupText = "Explain the Carat effect for a 0.1 carat increase",
    followupPayload = list(
      originalText = "Explain the Carat effect for a 0.1 carat increase",
      category = "unit_change_request",
      unitChangeResult = list(
        status = "ok",
        modelType = "lm",
        effectScale = "response_difference",
        predictorName = "Carat",
        requestedUnitChange = 0.1,
        oneUnitEffect = 1000,
        transformedEstimate = 100,
        confidenceInterval = list(lwr = 75, upr = 125)
      )
    ),
    assembledPrompt = "WMFM deterministic requested unit-change interpretation"
  )

  html = paste(capture.output(print(buildExplanationPromptDiagnosticsUi(diagnostics))), collapse = "\n")

  testthat::expect_match(html, "Requested unit-change predictor", fixed = TRUE)
  testthat::expect_match(html, "Requested unit change", fixed = TRUE)
  testthat::expect_match(html, "Original one-unit effect", fixed = TRUE)
  testthat::expect_match(html, "Transformed unit-change effect", fixed = TRUE)
  testthat::expect_match(html, "Transformed unit-change interval", fixed = TRUE)
})
