testthat::test_that("natural numeric GLM follow-up parser extracts in-range values", {
  data(course.df, package = "s20x")
  fit = stats::glm(Pass ~ Assign, data = course.df, family = stats::binomial())

  assignOut = computeGlmModelQuestionPrediction(
    model = fit,
    followupQuestion = "A student scored 15 on Assign. What probability of passing would you predict?"
  )

  testthat::expect_identical(assignOut$status, "ok")
  testthat::expect_equal(assignOut$resolvedPredictorValues$Assign, 15)
  testthat::expect_true(is.list(assignOut$confidenceInterval))
  testthat::expect_null(assignOut$predictionInterval)
  testthat::expect_null(assignOut$predictionIntervalUnsupportedReason)
})

testthat::test_that("logistic GLM follow-ups keep factor and numeric values", {
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

testthat::test_that("explicit logistic prediction interval requests return Bernoulli framing", {
  data(course.df, package = "s20x")
  fit = stats::glm(Pass ~ Assign, data = course.df, family = stats::binomial())

  out = computeGlmModelQuestionPrediction(
    model = fit,
    followupQuestion = "What is the prediction interval for a student with Assign = 15?"
  )

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$predictionType, "mean_response_prediction")
  testthat::expect_true(is.list(out$predictionInterval))
  testthat::expect_identical(out$predictionInterval$method, "bernoulli_outcome_framing")
  testthat::expect_equal(out$predictionInterval$outcomeProbabilities[["1"]], out$fittedPrediction)
  testthat::expect_equal(out$predictionInterval$outcomeProbabilities[["0"]], 1 - out$fittedPrediction)
  testthat::expect_null(out$predictionIntervalUnsupportedReason)
})

testthat::test_that("Poisson earthquake follow-ups predict at in-range Magnitude values", {
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
    followupQuestion = "What earthquake frequency would you expect for Magnitude = 5.4?"
  )

  testthat::expect_identical(outMagnitude$status, "ok")
  testthat::expect_equal(outMagnitude$resolvedPredictorValues$Magnitude, 5.4)
  testthat::expect_true(is.list(outMagnitude$confidenceInterval))
  testthat::expect_true(is.list(outMagnitude$predictionIntervalPolicy))
  testthat::expect_true(outMagnitude$predictionIntervalPolicy$supported)
  testthat::expect_null(outMagnitude$predictionInterval)

  fitMagnitudeLocn = stats::glm(Freq ~ Magnitude + Locn, data = quakeDf, family = stats::poisson())
  outMagnitudeLocn = computeGlmModelQuestionPrediction(
    model = fitMagnitudeLocn,
    followupQuestion = "What earthquake frequency would you expect for Magnitude = 5.4 and Locn = WA?"
  )

  testthat::expect_identical(outMagnitudeLocn$status, "ok")
  testthat::expect_equal(outMagnitudeLocn$resolvedPredictorValues$Magnitude, 5.4)
  testthat::expect_identical(outMagnitudeLocn$resolvedPredictorValues$Locn, "WA")
  testthat::expect_true(is.list(outMagnitudeLocn$confidenceInterval))
  testthat::expect_true(is.list(outMagnitudeLocn$predictionIntervalPolicy))
  testthat::expect_true(outMagnitudeLocn$predictionIntervalPolicy$supported)
  testthat::expect_null(outMagnitudeLocn$predictionInterval)
})



testthat::test_that("Poisson prediction-interval requests return conditional count intervals", {
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
  out = computeGlmModelQuestionPrediction(
    model = fitMagnitude,
    followupQuestion = "What prediction interval would you give for Magnitude = 5.4?"
  )

  testthat::expect_identical(out$status, "ok")
  testthat::expect_true(is.list(out$predictionInterval))
  testthat::expect_identical(out$predictionInterval$method, "conditional_poisson_quantile")
  testthat::expect_identical(out$predictionInterval$distribution, "poisson")
  testthat::expect_false(out$predictionInterval$parameterUncertaintyIncluded)
  testthat::expect_equal(out$predictionInterval$lwr, stats::qpois(0.025, out$fittedPrediction))
  testthat::expect_equal(out$predictionInterval$upr, stats::qpois(0.975, out$fittedPrediction))
})




testthat::test_that("logistic prediction-interval requests return Bernoulli outcome probabilities", {
  data(course.df, package = "s20x")
  fit = stats::glm(Pass ~ Assign, data = course.df, family = stats::binomial())

  out = computeGlmModelQuestionPrediction(
    model = fit,
    followupQuestion = "What prediction interval would you give for a student with Assign = 15?"
  )

  testthat::expect_identical(out$status, "ok")
  testthat::expect_true(is.list(out$predictionInterval))
  testthat::expect_identical(out$predictionInterval$method, "bernoulli_outcome_framing")
  testthat::expect_identical(out$predictionInterval$distribution, "bernoulli")
  testthat::expect_equal(out$predictionInterval$outcomeProbabilities[["1"]], out$fittedPrediction)
  testthat::expect_equal(out$predictionInterval$outcomeProbabilities[["0"]], 1 - out$fittedPrediction)
  testthat::expect_null(out$predictionIntervalUnsupportedReason)
})

testthat::test_that("GLM follow-up diagnostics JSON includes generated explanation text", {
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

testthat::test_that("GLM parser handles unnamed single numeric predictor values", {
  data(course.df, package = "s20x")
  fit = stats::glm(Pass ~ Assign, data = course.df, family = stats::binomial())

  out = computeGlmModelQuestionPrediction(
    model = fit,
    followupQuestion = "What would you predict for a student who scored 15?"
  )

  testthat::expect_identical(out$status, "ok")
  testthat::expect_equal(out$resolvedPredictorValues$Assign, 15)
  testthat::expect_identical(out$responseDescription, "probability")
})

testthat::test_that("GLM parser handles attendance-is-yes wording", {
  data(course.df, package = "s20x")
  fit = stats::glm(Pass ~ Attend, data = course.df, family = stats::binomial())

  payload = classifyModelFollowupQuestion("What happens if attendance is yes?")
  out = computeGlmModelQuestionPrediction(
    model = fit,
    followupQuestion = "What happens if attendance is yes?"
  )

  testthat::expect_identical(payload$category, "prediction_request")
  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$resolvedPredictorValues$Attend, "Yes")
})

testthat::test_that("GLM parser maps Washington location wording to WA", {
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
  fit = stats::glm(Freq ~ Magnitude + Locn, data = quakeDf, family = stats::poisson())

  out = computeGlmModelQuestionPrediction(
    model = fit,
    followupQuestion = "How many earthquakes would you expect in Washington at magnitude 5.6?"
  )

  testthat::expect_identical(out$status, "ok")
  testthat::expect_equal(out$resolvedPredictorValues$Magnitude, 5.6)
  testthat::expect_identical(out$resolvedPredictorValues$Locn, "WA")
})
