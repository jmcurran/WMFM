test_that("score.wmfmGrade stores method specific deterministic results", {
  skip_if_not_installed("testthat")

  df = data.frame(
    y = c(1, 2, 3, 4, 5),
    x = c(0, 1, 2, 3, 4)
  )

  model = stats::lm(y ~ x, data = df)

  wm = newWmfmModel(
    model = model,
    formula = y ~ x,
    modelType = "lm",
    data = df,
    equations = "y = a + bx",
    explanation = "Each one-unit increase in x is associated with an average increase in y."
  )

  g = grade(
    wm,
    explanation = "Each one-unit increase in x is associated with about a one-point increase in y.",
    score = FALSE
  )

  g = score(g)

  expect_true("deterministic" %in% names(g$scores$byMethod))
  expect_equal(g$meta$lastScoredMethod, "deterministic")
  expect_true(is.data.frame(g$scores$byMethod$deterministic$student))
})

test_that("score.wmfmGrade accepts llm method with a mock chat provider", {
  skip_if_not_installed("testthat")

  df = data.frame(
    y = c(1, 2, 3, 4, 5),
    x = c(0, 1, 2, 3, 4)
  )

  model = stats::lm(y ~ x, data = df)

  wm = newWmfmModel(
    model = model,
    formula = y ~ x,
    modelType = "lm",
    data = df,
    equations = "y = a + bx",
    explanation = "Each one-unit increase in x is associated with an average increase in y."
  )

  mockChat = structure(
    list(
      chat = function(prompt) {
        paste(
          '{',
          '"effectDirectionCorrect": 2,',
          '"effectScaleAppropriate": 2,',
          '"referenceGroupHandledCorrectly": 2,',
          '"interactionCoverageAdequate": 2,',
          '"interactionSubstantiveCorrect": 2,',
          '"uncertaintyHandlingAppropriate": 1,',
          '"inferentialRegisterAppropriate": 2,',
          '"mainEffectCoverageAdequate": 2,',
          '"referenceGroupCoverageAdequate": 2,',
          '"clarityAdequate": 2,',
          '"numericExpressionAdequate": 1,',
          '"comparisonStructureClear": 2,',
          '"fatalFlawDetected": false,',
          '"factualScore": 1.5,',
          '"inferenceScore": 2.0,',
          '"completenessScore": 1.5,',
          '"clarityScore": 2.0,',
          '"calibrationScore": 2.0,',
          '"overallScore": 85.0,',
          '"overallPass": true,',
          '"llmScoringSummary": "Mock score.",',
          '"fieldReasons": {',
          '"effectDirectionCorrect": "Direction was described correctly.",',
          '"effectScaleAppropriate": "Scale was described adequately.",',
          '"referenceGroupHandledCorrectly": "Reference group was handled adequately.",',
          '"interactionCoverageAdequate": "No interaction issue.",',
          '"interactionSubstantiveCorrect": "No interaction issue.",',
          '"uncertaintyHandlingAppropriate": "Uncertainty handling could be stronger.",',
          '"inferentialRegisterAppropriate": "Register was appropriate.",',
          '"mainEffectCoverageAdequate": "Main effect was covered.",',
          '"referenceGroupCoverageAdequate": "Reference-group context was fine.",',
          '"clarityAdequate": "Clarity was adequate.",',
          '"numericExpressionAdequate": "Numeric effect size was present but not fully developed.",',
          '"comparisonStructureClear": "Comparison structure was clear.",',
          '"fatalFlawDetected": "No fatal flaw."',
          '}',
          '}',
          sep = "\n"
        )
      }
    ),
    class = "mockChatProvider"
  )

  g = grade(
    wm,
    explanation = "Each one-unit increase in x is associated with about a one-point increase in y.",
    score = FALSE
  )

  g = score(g, method = "llm", chat = mockChat, showProgress = FALSE)

  expect_true("llm" %in% names(g$scores$byMethod))
  expect_equal(g$meta$lastScoredMethod, "llm")
  expect_equal(g$scores$byMethod$llm$overallScore, 85)
})
