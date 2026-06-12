testthat::test_that("buildWmfmLlmScoringUserPrompt includes key scoring context", {
  prompt = buildWmfmLlmScoringUserPrompt(makeRawRunRecordForScoring())

  testthat::expect_match(prompt, "MODEL CONTEXT")
  testthat::expect_match(prompt, "EXPLANATION TO SCORE")
  testthat::expect_match(prompt, "effectDirectionCorrect")
  testthat::expect_match(prompt, "fieldReasons")
})

testthat::test_that("buildWmfmLlmScoringUserPrompt validates required fields", {
  x = makeRawRunRecordForScoring()
  x$formula = NULL

  testthat::expect_error(
    buildWmfmLlmScoringUserPrompt(x),
    "missing required fields"
  )
})


testthat::test_that("buildWmfmLlmScoringUserPrompt reports interaction context when present", {
  x = makeRawRunRecordForScoring(
    interactionTerms = c("Attend:Group"),
    interactionMinPValue = 0.012
  )

  prompt = buildWmfmLlmScoringUserPrompt(x)

  testthat::expect_match(prompt, "Interaction terms are present")
  testthat::expect_match(prompt, "Attend:Group")
  testthat::expect_match(prompt, "0.012")
})

testthat::test_that("buildWmfmLlmScoringUserPrompt includes adjustment-aware scoring policy when supplied", {
  x = makeRawRunRecordForScoring()
  x$adjustmentVariables = "picture"
  x$primaryVariables = "gender"

  prompt = buildWmfmLlmScoringUserPrompt(x)

  testthat::expect_match(prompt, "ADJUSTMENT CONTEXT", fixed = TRUE)
  testthat::expect_match(prompt, "Adjustment variables: picture", fixed = TRUE)
  testthat::expect_match(prompt, "Variables of scientific interest: gender", fixed = TRUE)
  testthat::expect_match(prompt, "Do not penalise explanations for omitting adjustment-level details", fixed = TRUE)
  testthat::expect_match(prompt, "do not penalise the absence of interaction cell-by-cell descriptions", ignore.case = TRUE)
})



testthat::test_that("buildWmfmLlmScoringUserPrompt rewards restrained adjustment-aware explanations", {
  x = makeRawRunRecordForScoring(
    formula = "arousal ~ gender + picture + gender:picture",
    interactionTerms = "gender:picture",
    interactionMinPValue = 0.18,
    adjustmentVariables = "picture",
    primaryVariables = "gender"
  )

  prompt = buildWmfmLlmScoringUserPrompt(x)

  testthat::expect_match(prompt, "Reward restrained explanations", fixed = TRUE)
  testthat::expect_match(prompt, "using the variables of scientific interest", fixed = TRUE)
  testthat::expect_match(prompt, "Do not require coefficient-by-coefficient narration", fixed = TRUE)
  testthat::expect_match(prompt, "should not become narrative conditioning axes", fixed = TRUE)
  testthat::expect_match(prompt, "recite the regression table instead of answering the research question", fixed = TRUE)
})

testthat::test_that("buildWmfmLlmScoringUserPrompt maps adjustment policy to rubric fields", {
  x = makeRawRunRecordForScoring(
    formula = "arousal ~ gender + picture + gender:picture",
    interactionTerms = "gender:picture",
    adjustmentVariables = "picture",
    primaryVariables = "gender"
  )

  prompt = buildWmfmLlmScoringUserPrompt(x)

  testthat::expect_match(prompt, "do not mark this field down merely because", fixed = TRUE)
  testthat::expect_match(prompt, "adjustment-level interaction cell narration", fixed = TRUE)
  testthat::expect_match(prompt, "do not require separate substantive narration of adjustment-variable main effects", fixed = TRUE)
  testthat::expect_match(prompt, "do not require adjustment-level reference-group narration", fixed = TRUE)
  testthat::expect_match(prompt, "important primary model effects", fixed = TRUE)
  testthat::expect_match(prompt, "do not require adjustment-level comparison structures", fixed = TRUE)
})

testthat::test_that("buildWmfmLlmScoringUserPrompt keeps default guidance when no adjustment variables are supplied", {
  prompt = buildWmfmLlmScoringUserPrompt(makeRawRunRecordForScoring())

  testthat::expect_match(prompt, "ADJUSTMENT CONTEXT", fixed = TRUE)
  testthat::expect_match(prompt, "No adjustment-variable context was supplied", fixed = TRUE)
})

testthat::test_that("buildWmfmLlmScoringUserPrompt includes follow-up scoring context", {
  x = makeRawRunRecordForScoring()
  x$hasFollowupScoringContext = TRUE
  x$followupQuestion = "Predict a future count when x is 4"
  x$followupCategory = "prediction_interval_request"
  x$followupPredictionStatus = "ok"
  x$followupPredictionType = "individual_prediction_interval"
  x$followupIntervalType = "prediction_interval"
  x$followupFutureObservationType = "future_count"
  x$followupExtrapolationStatus = "extrapolation_warning"
  x$followupExtrapolationExplanation = "WMFM computed the prediction with slight extrapolation."
  x$followupParameterUncertaintyIncluded = FALSE

  prompt = buildWmfmLlmScoringUserPrompt(x)

  testthat::expect_match(prompt, "FOLLOW-UP SCORING CONTEXT", fixed = TRUE)
  testthat::expect_match(prompt, "Prediction type: individual_prediction_interval", fixed = TRUE)
  testthat::expect_match(prompt, "Interval type: prediction_interval", fixed = TRUE)
  testthat::expect_match(prompt, "Future-observation type: future_count", fixed = TRUE)
  testthat::expect_match(prompt, "Extrapolation status: extrapolation_warning", fixed = TRUE)
  testthat::expect_match(prompt, "Parameter uncertainty included: FALSE", fixed = TRUE)
  testthat::expect_match(prompt, "future-observation prediction intervals", fixed = TRUE)
})

testthat::test_that("buildWmfmLlmScoringUserPrompt maps follow-up policy to rubric fields", {
  x = makeRawRunRecordForScoring(
    hasFollowupScoringContext = TRUE,
    followupQuestion = "Give a prediction interval for a future count when x is 4",
    followupCategory = "prediction_interval_request",
    followupPredictionStatus = "ok",
    followupPredictionType = "individual_prediction_interval",
    followupIntervalType = "prediction_interval",
    followupFutureObservationType = "poisson_count",
    followupExtrapolationStatus = "within_range",
    followupExtrapolationExplanation = "No extrapolation was needed.",
    followupParameterUncertaintyIncluded = FALSE
  )

  prompt = buildWmfmLlmScoringUserPrompt(x)

  testthat::expect_match(prompt, "Use uncertaintyHandlingAppropriate", fixed = TRUE)
  testthat::expect_match(prompt, "Use comparisonStructureClear", fixed = TRUE)
  testthat::expect_match(prompt, "confidence-interval wording for a prediction interval", fixed = TRUE)
  testthat::expect_match(prompt, "Bernoulli outcome", fixed = TRUE)
  testthat::expect_match(prompt, "parameter uncertainty is included", fixed = TRUE)
})
