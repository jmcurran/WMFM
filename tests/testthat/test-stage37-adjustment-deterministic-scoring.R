testthat::test_that("deterministic scoring rewards restrained adjustment-aware interaction explanations", {
  explanationText = paste(
    "Researchers asked whether arousal levels differ between females and males.",
    "The analysis adjusted for the picture shown to participants.",
    "After adjusting for picture effects, the results do not indicate a clear gender difference in arousal.",
    "This keeps the picture variable in the background rather than narrating picture-level fitted means."
  )

  runsDf = data.frame(
    runId = 1L,
    exampleName = "Arousal",
    package = "s20x",
    modelType = "lm",
    formula = "arousal ~ gender + picture + gender:picture",
    explanationText = explanationText,
    hasInteractionTerms = TRUE,
    nInteractionTerms = 1L,
    interactionTerms = "gender:picture",
    interactionMinPValue = 0.18,
    interactionAlpha = 0.05,
    hasFactorPredictors = TRUE,
    adjustmentVariables = "picture",
    primaryVariables = "gender",
    hasAdjustmentVariables = TRUE,
    hasPrimaryVariables = TRUE,
    comparisonLanguageMention = TRUE,
    conditionalLanguageMention = FALSE,
    interactionMention = FALSE,
    uncertaintyMention = TRUE,
    ciMention = FALSE,
    outcomeMention = TRUE,
    predictorMention = TRUE,
    effectDirectionClaim = "mixed_or_both",
    effectScaleClaim = "additive",
    interactionSubstantiveClaim = "no_clear_difference",
    inferentialRegister = "inferential",
    overclaimDetected = FALSE,
    underclaimDetected = FALSE,
    stringsAsFactors = FALSE
  )

  scoredDf = scoreWmfmRepeatedRuns(runsDf, penaliseDuplicates = FALSE)

  testthat::expect_identical(scoredDf$interactionCoverageAdequate, 2L)
  testthat::expect_identical(scoredDf$interactionSubstantiveCorrect, 2L)
  testthat::expect_identical(scoredDf$numericExpressionAdequate, 2L)
  testthat::expect_identical(scoredDf$comparisonStructureClear, 2L)
  testthat::expect_false(scoredDf$fatalFlawDetected)
})

testthat::test_that("deterministic numeric restraint is limited to supplied adjustment context", {
  explanationText = paste(
    "Researchers asked whether arousal levels differ between females and males.",
    "The results do not indicate a clear gender difference in arousal.",
    "This statement is restrained but it is not marked as adjusted for a background variable."
  )

  runsDf = data.frame(
    runId = 2L,
    exampleName = "Arousal",
    package = "s20x",
    modelType = "lm",
    formula = "arousal ~ gender + picture + gender:picture",
    explanationText = explanationText,
    hasInteractionTerms = TRUE,
    nInteractionTerms = 1L,
    interactionTerms = "gender:picture",
    interactionMinPValue = 0.18,
    interactionAlpha = 0.05,
    hasFactorPredictors = TRUE,
    adjustmentVariables = "",
    primaryVariables = "gender",
    hasAdjustmentVariables = FALSE,
    hasPrimaryVariables = TRUE,
    comparisonLanguageMention = TRUE,
    conditionalLanguageMention = FALSE,
    interactionMention = FALSE,
    uncertaintyMention = TRUE,
    ciMention = FALSE,
    outcomeMention = TRUE,
    predictorMention = TRUE,
    effectDirectionClaim = "mixed_or_both",
    effectScaleClaim = "additive",
    interactionSubstantiveClaim = "no_clear_difference",
    inferentialRegister = "inferential",
    overclaimDetected = FALSE,
    underclaimDetected = FALSE,
    stringsAsFactors = FALSE
  )

  scoredDf = scoreWmfmRepeatedRuns(runsDf, penaliseDuplicates = FALSE)

  testthat::expect_equal(scoredDf$numericExpressionAdequate, 0L)
})
