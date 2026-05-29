testthat::test_that("Stage 25.3 natural numeric follow-up parser extracts in-range values", {
  data(course.df, package = "s20x")
  fit = stats::glm(Pass ~ Assign, data = course.df, family = stats::binomial())

  assignOut = computeGlmModelQuestionPrediction(
    model = fit,
    followupQuestion = "A student scored 15 on Assign. What probability of passing would you predict?"
  )

  testthat::expect_identical(assignOut$status, "ok")
  testthat::expect_equal(assignOut$resolvedPredictorValues$Assign, 15)
  testthat::expect_true(is.list(assignOut$confidenceInterval))
  testthat::expect_match(
    assignOut$predictionIntervalUnsupportedReason,
    "not currently supported",
    fixed = TRUE
  )
})

testthat::test_that("Stage 25.3 logistic follow-ups keep factor and numeric values", {
  data(course.df, package = "s20x")
  fit = stats::glm(Pass ~ Attend + Test, data = course.df, family = stats::binomial())

  out = computeGlmModelQuestionPrediction(
    model = fit,
    followupQuestion = "Suppose a student has Test = 10 and Attend = Yes. What probability of passing would you predict?"
  )

  testthat::expect_identical(out$status, "ok")
  testthat::expect_equal(out$resolvedPredictorValues$Test, 10)
  testthat::expect_identical(out$resolvedPredictorValues$Attend, "Yes")
  testthat::expect_true(is.list(out$confidenceInterval))
  testthat::expect_identical(out$responseDescription, "probability")
})

testthat::test_that("Stage 25.3 explicit GLM prediction interval requests still fail safely", {
  data(course.df, package = "s20x")
  fit = stats::glm(Pass ~ Assign, data = course.df, family = stats::binomial())

  out = computeGlmModelQuestionPrediction(
    model = fit,
    followupQuestion = "What is the prediction interval for a student with Assign = 15?"
  )

  testthat::expect_identical(out$status, "unsupported")
  testthat::expect_identical(out$reason, "unsupported_glm_interval_request")
})

testthat::test_that("Stage 25.3 earthquake follow-ups predict at Magnitude 3", {
  quakePath = system.file(
    "extdata",
    "examples",
    "Quakes",
    "Quakes.df.rda",
    package = "WMFM"
  )
  testthat::skip_if_not(file.exists(quakePath), "quake example data is unavailable")

  loadEnv = new.env(parent = emptyenv())
  objectNames = load(quakePath, envir = loadEnv)
  quakeDf = loadEnv[[objectNames[[1]]]]

  fitMagnitude = stats::glm(Freq ~ Magnitude, data = quakeDf, family = stats::poisson())
  outMagnitude = computeGlmModelQuestionPrediction(
    model = fitMagnitude,
    followupQuestion = "What earthquake frequency would you expect for Magnitude = 3?"
  )

  testthat::expect_identical(outMagnitude$status, "ok")
  testthat::expect_equal(outMagnitude$resolvedPredictorValues$Magnitude, 3)
  testthat::expect_true(is.list(outMagnitude$confidenceInterval))
  testthat::expect_match(
    outMagnitude$predictionIntervalUnsupportedReason,
    "not currently supported",
    fixed = TRUE
  )

  fitMagnitudeLocn = stats::glm(Freq ~ Magnitude + Locn, data = quakeDf, family = stats::poisson())
  outMagnitudeLocn = computeGlmModelQuestionPrediction(
    model = fitMagnitudeLocn,
    followupQuestion = "What earthquake frequency would you expect for Magnitude = 3 and Locn = WA?"
  )

  testthat::expect_identical(outMagnitudeLocn$status, "ok")
  testthat::expect_equal(outMagnitudeLocn$resolvedPredictorValues$Magnitude, 3)
  testthat::expect_identical(outMagnitudeLocn$resolvedPredictorValues$Locn, "WA")
  testthat::expect_true(is.list(outMagnitudeLocn$confidenceInterval))
  testthat::expect_match(
    outMagnitudeLocn$predictionIntervalUnsupportedReason,
    "not currently supported",
    fixed = TRUE
  )
})

testthat::test_that("Stage 25.3 diagnostics JSON includes generated explanation text", {
  diagnosticsJson = buildExplanationPromptDiagnosticsJson(list(
    followupPayload = list(
      originalText = "What is the predicted probability when Assign = 15?",
      category = "prediction_request",
      predictionResult = list(
        status = "ok",
        modelType = "glm",
        glmFamily = "binomial",
        glmLink = "logit",
        responseScale = "response",
        responseDescription = "probability"
      )
    ),
    assembledPrompt = "Prompt text",
    generatedExplanation = "Generated explanation text"
  ))
  diagnostics = jsonlite::fromJSON(diagnosticsJson, simplifyVector = FALSE)

  testthat::expect_identical(diagnostics$generatedExplanation, "Generated explanation text")
})
