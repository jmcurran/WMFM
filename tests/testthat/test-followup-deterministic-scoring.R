testthat::test_that("follow-up scoring rewards correct prediction interval framing", {
  rawRecord = data.frame(
    explanationText = paste(
      "This is a prediction interval for a future observation.",
      "It describes uncertainty about a future count and does not include parameter uncertainty."
    ),
    hasFollowupScoringContext = TRUE,
    followupPredictionStatus = "ok",
    followupPredictionType = "individual_prediction_interval",
    followupIntervalType = "prediction_interval",
    followupFutureObservationType = "poisson_count",
    followupExtrapolationStatus = NA_character_,
    followupParameterUncertaintyIncluded = FALSE,
    followupPredictionTypeClaim = "future_observation",
    followupIntervalTypeClaim = "prediction_interval",
    followupExtrapolationWarningMention = FALSE,
    followupBlockedPredictionMention = FALSE,
    followupFutureOutcomeFramingClaim = "none",
    followupParameterUncertaintyExclusionMention = TRUE,
    effectDirectionClaim = "not_stated",
    effectScaleClaim = "not_stated",
    uncertaintyMention = TRUE,
    ciMention = FALSE,
    usesInferentialLanguage = TRUE,
    overclaimDetected = FALSE,
    hasInteractionTerms = FALSE,
    hasFactorPredictors = FALSE,
    explanationPresent = TRUE,
    wordCount = 16L,
    stringsAsFactors = FALSE
  )

  scored = scoreWmfmRunRecordsCore(rawRecord, penaliseDuplicates = FALSE)

  testthat::expect_equal(scored$uncertaintyHandlingAppropriate, 2L)
  testthat::expect_equal(scored$comparisonStructureClear, 2L)
  testthat::expect_false(scored$fatalFlawDetected)
})


testthat::test_that("follow-up scoring penalises confidence interval wording for prediction intervals", {
  rawRecord = data.frame(
    explanationText = paste(
      "This confidence interval describes the fitted mean response.",
      "It gives uncertainty about the expected count."
    ),
    hasFollowupScoringContext = TRUE,
    followupPredictionStatus = "ok",
    followupPredictionType = "individual_prediction_interval",
    followupIntervalType = "prediction_interval",
    followupFutureObservationType = "poisson_count",
    followupExtrapolationStatus = NA_character_,
    followupParameterUncertaintyIncluded = FALSE,
    followupPredictionTypeClaim = "fitted_mean",
    followupIntervalTypeClaim = "confidence_interval",
    followupExtrapolationWarningMention = FALSE,
    followupBlockedPredictionMention = FALSE,
    followupFutureOutcomeFramingClaim = "none",
    followupParameterUncertaintyExclusionMention = FALSE,
    effectDirectionClaim = "not_stated",
    effectScaleClaim = "not_stated",
    uncertaintyMention = TRUE,
    ciMention = TRUE,
    usesInferentialLanguage = TRUE,
    overclaimDetected = FALSE,
    hasInteractionTerms = FALSE,
    hasFactorPredictors = FALSE,
    explanationPresent = TRUE,
    wordCount = 14L,
    stringsAsFactors = FALSE
  )

  scored = scoreWmfmRunRecordsCore(rawRecord, penaliseDuplicates = FALSE)

  testthat::expect_equal(scored$uncertaintyHandlingAppropriate, 0L)
  testthat::expect_true(scored$fatalFlawDetected)
})


testthat::test_that("logistic follow-up scoring rejects continuous individual probability intervals", {
  rawRecord = data.frame(
    explanationText = paste(
      "This gives a probability interval for an individual.",
      "The individual result is treated as if it could vary continuously."
    ),
    hasFollowupScoringContext = TRUE,
    followupPredictionStatus = "ok",
    followupPredictionType = "future_observation",
    followupIntervalType = NA_character_,
    followupFutureObservationType = "bernoulli",
    followupExtrapolationStatus = NA_character_,
    followupParameterUncertaintyIncluded = FALSE,
    followupPredictionTypeClaim = "future_observation",
    followupIntervalTypeClaim = "none",
    followupExtrapolationWarningMention = FALSE,
    followupBlockedPredictionMention = FALSE,
    followupFutureOutcomeFramingClaim = "continuous_interval",
    followupParameterUncertaintyExclusionMention = FALSE,
    effectDirectionClaim = "not_stated",
    effectScaleClaim = "probability_or_odds",
    uncertaintyMention = TRUE,
    ciMention = FALSE,
    usesInferentialLanguage = TRUE,
    overclaimDetected = FALSE,
    hasInteractionTerms = FALSE,
    hasFactorPredictors = FALSE,
    explanationPresent = TRUE,
    wordCount = 17L,
    stringsAsFactors = FALSE
  )

  scored = scoreWmfmRunRecordsCore(rawRecord, penaliseDuplicates = FALSE)

  testthat::expect_equal(scored$comparisonStructureClear, 0L)
  testthat::expect_true(scored$fatalFlawDetected)
})


testthat::test_that("follow-up scoring rewards extrapolation and blocked prediction warnings", {
  rawRecord = data.frame(
    explanationText = paste(
      "The prediction was blocked because it is beyond the configured tolerance.",
      "This should not be treated as an ordinary fitted value."
    ),
    hasFollowupScoringContext = TRUE,
    followupPredictionStatus = "blocked",
    followupPredictionType = NA_character_,
    followupIntervalType = NA_character_,
    followupFutureObservationType = NA_character_,
    followupExtrapolationStatus = NA_character_,
    followupParameterUncertaintyIncluded = FALSE,
    followupPredictionTypeClaim = "none",
    followupIntervalTypeClaim = "none",
    followupExtrapolationWarningMention = FALSE,
    followupBlockedPredictionMention = TRUE,
    followupFutureOutcomeFramingClaim = "none",
    followupParameterUncertaintyExclusionMention = FALSE,
    effectDirectionClaim = "not_stated",
    effectScaleClaim = "not_stated",
    uncertaintyMention = TRUE,
    ciMention = FALSE,
    usesInferentialLanguage = TRUE,
    overclaimDetected = FALSE,
    hasInteractionTerms = FALSE,
    hasFactorPredictors = FALSE,
    explanationPresent = TRUE,
    wordCount = 17L,
    stringsAsFactors = FALSE
  )

  scored = scoreWmfmRunRecordsCore(rawRecord, penaliseDuplicates = FALSE)

  testthat::expect_equal(scored$uncertaintyHandlingAppropriate, 2L)
  testthat::expect_false(scored$fatalFlawDetected)
})
