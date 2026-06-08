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
