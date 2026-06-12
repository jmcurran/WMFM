testthat::test_that("buildWmfmRunRecord builds a raw run record without score fields", {
  out = buildWmfmRunRecord(
    runId = 1L,
    exampleName = "Course",
    package = "WMFM",
    modelType = "lm",
    formula = "Exam ~ Attend",
    equationsText = "Exam = 6.62 + 3.52 * Attend",
    explanationText = paste(
      "Attendance is associated with higher exam scores.",
      "Compared with the baseline, scores increase by about 3.5 points."
    ),
    interactionTerms = character(0),
    interactionMinPValue = NA_real_,
    interactionAlpha = 0.05
  )

  testthat::expect_identical(out$runId, 1L)
  testthat::expect_identical(out$exampleName, "Course")
  testthat::expect_identical(out$package, "WMFM")
  testthat::expect_identical(out$modelType, "lm")
  testthat::expect_identical(out$hasInteractionTerms, FALSE)
  testthat::expect_identical(out$nInteractionTerms, 0L)
  testthat::expect_true(is.logical(out$comparisonLanguageMention))
  testthat::expect_true(is.numeric(out$wordCount))
  testthat::expect_true(is.numeric(out$sentenceCount))
  testthat::expect_identical(out$hasFollowupScoringContext, FALSE)
  testthat::expect_true(is.na(out$followupQuestion))
  testthat::expect_s3_class(
    as.data.frame(out, stringsAsFactors = FALSE),
    "data.frame"
  )

  testthat::expect_false(any(c(
    "effectDirectionCorrect",
    "overallScore",
    "overallPass",
    "llmScored"
  ) %in% names(out)))
})

testthat::test_that("buildWmfmRunRecord records interaction metadata separately", {
  out = buildWmfmRunRecord(
    runId = 2L,
    exampleName = "Course",
    package = "WMFM",
    modelType = "lm",
    formula = "Exam ~ Attend * Group",
    equationsText = "Exam = 6.62 + 3.52 * Attend + Group + Attend:Group",
    explanationText = "The effect varies by group and is steeper for Group B.",
    interactionTerms = c("Attend:Group"),
    interactionMinPValue = 0.012,
    interactionAlpha = 0.05
  )

  testthat::expect_identical(out$hasInteractionTerms, TRUE)
  testthat::expect_identical(out$nInteractionTerms, 1L)
  testthat::expect_match(out$interactionTerms, "Attend:Group")
  testthat::expect_identical(out$interactionSubstantiveClaim, "difference_claimed_strongly")
})

testthat::test_that("buildWmfmRunRecord records follow-up scoring context and claims", {
  out = buildWmfmRunRecord(
    runId = 3L,
    exampleName = "Course",
    package = "WMFM",
    modelType = "glm",
    formula = "Pass ~ Attend",
    equationsText = "logit(Pr(Pass)) = -1 + 0.5 * Attend",
    explanationText = paste(
      "For an individual outcome, WMFM reports the two possible outcomes",
      "as Bernoulli outcome probabilities.",
      "This is conditional on the fitted model and does not include parameter uncertainty."
    ),
    followupScoringContext = list(
      followupQuestion = "What is the chance of passing when Attend is high?",
      followupCategory = "prediction_interval_request",
      followupPredictionStatus = "ok",
      followupPredictionType = "individual_prediction_interval",
      followupIntervalType = NA_character_,
      followupFutureObservationType = "future_binary_outcome",
      followupExtrapolationStatus = "in_range",
      followupParameterUncertaintyIncluded = FALSE
    )
  )

  testthat::expect_identical(out$hasFollowupScoringContext, TRUE)
  testthat::expect_identical(out$followupPredictionType, "individual_prediction_interval")
  testthat::expect_identical(out$followupFutureObservationType, "future_binary_outcome")
  testthat::expect_identical(
    out$followupFutureOutcomeFramingClaim,
    "bernoulli_outcome_probabilities"
  )
  testthat::expect_true(out$followupParameterUncertaintyExclusionMention)
})

testthat::test_that("rebuildWmfmRunRecords preserves follow-up scoring context", {
  runRecord = buildWmfmRunRecord(
    runId = 4L,
    exampleName = "Counts",
    package = "WMFM",
    modelType = "glm",
    formula = "Freq ~ Magnitude",
    equationsText = "log(E(Freq)) = 5 - 0.7 * Magnitude",
    researchQuestion = "Predict a future earthquake count.",
    explanationText = paste(
      "This prediction interval describes uncertainty for a future count.",
      "It does not include parameter uncertainty."
    ),
    followupScoringContext = list(
      followupQuestion = "Give a prediction interval for Magnitude = 5.4.",
      followupCategory = "prediction_interval_request",
      followupPredictionStatus = "ok",
      followupPredictionType = "individual_prediction_interval",
      followupIntervalType = "prediction_interval",
      followupFutureObservationType = "poisson_count",
      followupExtrapolationStatus = "in_range",
      followupExtrapolationExplanation = "No extrapolation was needed.",
      followupParameterUncertaintyIncluded = FALSE
    )
  )

  x = structure(
    list(
      runs = list(runRecord),
      summary = data.frame(dummy = 1)
    ),
    class = c("wmfmRuns", "list")
  )

  rebuilt = rebuildWmfmRunRecords(x)
  rebuiltRecord = rebuilt$runs[[1]]

  testthat::expect_null(rebuilt$summary)
  testthat::expect_identical(rebuiltRecord$hasFollowupScoringContext, TRUE)
  testthat::expect_identical(
    rebuiltRecord$followupPredictionType,
    "individual_prediction_interval"
  )
  testthat::expect_identical(
    rebuiltRecord$followupIntervalType,
    "prediction_interval"
  )
  testthat::expect_identical(
    rebuiltRecord$followupFutureObservationType,
    "poisson_count"
  )
  testthat::expect_identical(
    rebuiltRecord$followupExtrapolationExplanation,
    "No extrapolation was needed."
  )
  testthat::expect_identical(
    rebuiltRecord$followupParameterUncertaintyIncluded,
    FALSE
  )
  testthat::expect_identical(
    rebuiltRecord$followupParameterUncertaintyExclusionMention,
    TRUE
  )
  testthat::expect_identical(
    rebuiltRecord$researchQuestion,
    "Predict a future earthquake count."
  )
})


testthat::test_that("buildWmfmRunRecord records adjustment and primary variable metadata", {
  out = buildWmfmRunRecord(
    runId = 5L,
    exampleName = "Arousal",
    package = "WMFM",
    modelType = "lm",
    formula = "arousal ~ gender + picture + gender:picture",
    equationsText = "arousal = model with gender and picture",
    explanationText = paste(
      "After adjusting for picture effects,",
      "the data do not indicate a clear gender difference."
    ),
    interactionTerms = "gender:picture",
    adjustmentVariables = c("picture", "picture", ""),
    primaryVariables = "gender"
  )

  testthat::expect_identical(out$adjustmentVariables, "picture")
  testthat::expect_identical(out$hasAdjustmentVariables, TRUE)
  testthat::expect_identical(out$nAdjustmentVariables, 1L)
  testthat::expect_identical(out$primaryVariables, "gender")
  testthat::expect_identical(out$hasPrimaryVariables, TRUE)
  testthat::expect_identical(out$nPrimaryVariables, 1L)
})
