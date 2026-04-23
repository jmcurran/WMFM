testthat::test_that("score.wmfmRuns returns wmfmScores for deterministic scoring", {
  x = makeFakeWmfmRuns()

  fakeDeterministic = data.frame(
    effectDirectionCorrect = c(2L, 2L, 1L),
    effectScaleAppropriate = c(2L, 2L, 2L),
    referenceGroupHandledCorrectly = c(2L, 2L, 2L),
    interactionCoverageAdequate = c(2L, 2L, 2L),
    interactionSubstantiveCorrect = c(2L, 2L, 2L),
    uncertaintyHandlingAppropriate = c(1L, 1L, 1L),
    inferentialRegisterAppropriate = c(2L, 2L, 2L),
    mainEffectCoverageAdequate = c(2L, 2L, 2L),
    referenceGroupCoverageAdequate = c(2L, 2L, 2L),
    clarityAdequate = c(2L, 2L, 2L),
    numericExpressionAdequate = c(2L, 2L, 2L),
    comparisonStructureClear = c(2L, 2L, 2L),
    fatalFlawDetected = c(FALSE, FALSE, FALSE),
    factualScore = c(1.5, 1.6, 1.4),
    inferenceScore = c(1.3, 1.2, 1.1),
    completenessScore = c(1.6, 1.7, 1.5),
    clarityScore = c(1.8, 1.7, 1.6),
    calibrationScore = c(1.2, 1.1, 1.0),
    overallScore = c(80, 82, 75),
    overallPass = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    scoreWmfmRepeatedRuns = function(runsDf) {
      fakeDeterministic
    },
    .package = "WMFM"
  )

  out = score.wmfmRuns(
    x = x,
    method = "deterministic"
  )

  testthat::expect_s3_class(out, "wmfmScores")
  testthat::expect_identical(out$methods, "deterministic")
  testthat::expect_length(out$scores$deterministic, 3)
  testthat::expect_identical(out$scores$deterministic[[1]]$primaryScoringMethod, "deterministic")
})


testthat::test_that("score.wmfmRuns returns wmfmScores for mocked llm scoring", {
  x = makeFakeWmfmRuns()

  fakeLlmScores = replicate(
    3,
    simplify = FALSE,
    expr = list(
      effectDirectionCorrect = 2L,
      effectScaleAppropriate = 2L,
      referenceGroupHandledCorrectly = 2L,
      interactionCoverageAdequate = 2L,
      interactionSubstantiveCorrect = 2L,
      uncertaintyHandlingAppropriate = 1L,
      inferentialRegisterAppropriate = 2L,
      mainEffectCoverageAdequate = 2L,
      referenceGroupCoverageAdequate = 2L,
      clarityAdequate = 2L,
      numericExpressionAdequate = 2L,
      comparisonStructureClear = 2L,
      fatalFlawDetected = FALSE,
      factualScore = 1.8,
      inferenceScore = 1.4,
      completenessScore = 1.7,
      clarityScore = 1.9,
      calibrationScore = 1.3,
      overallScore = 90,
      overallPass = TRUE,
      llmScored = TRUE,
      llmScoringModel = "ProviderFake",
      llmScoringRaw = "{}",
      llmScoringSummary = "Mostly correct."
    )
  )

  testthat::local_mocked_bindings(
    scoreWmfmRunsWithLlm = function(runRecords, chat, useCache, showProgress, verbose) {
      attr(fakeLlmScores, "timing") = list(
        startedAt = as.character(Sys.time()),
        finishedAt = as.character(Sys.time()),
        averageIterationSeconds = 0.01,
        iterationSeconds = c(0.01, 0.01, 0.01)
      )
      fakeLlmScores
    },
    .package = "WMFM"
  )

  out = score.wmfmRuns(
    x = x,
    method = "llm",
    chat = makeFakeChat("{}"),
    showProgress = FALSE
  )

  testthat::expect_s3_class(out, "wmfmScores")
  testthat::expect_identical(out$methods, "llm")
  testthat::expect_length(out$scores$llm, 3)
  testthat::expect_identical(out$scores$llm[[1]]$primaryScoringMethod, "llm")
  testthat::expect_identical(out$meta$llmModel, "ProviderFake")
})
