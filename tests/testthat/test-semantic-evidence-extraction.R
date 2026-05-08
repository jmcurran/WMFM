test_that("semantic evidence recognises SG-3 gender comparison paraphrases", {
  fixtures = readDeveloperScoringFixtures()
  fixture = fixtures[["test-SG-3"]]
  explanation = fixture$repeated$runs[[2]]$explanation

  evidence = extractWmfmSemanticEvidence(explanation, fixture$appState)

  expect_equal(evidence$effectDirection, "positive")
  expect_equal(evidence$effectScale, "additive")
  expect_true(evidence$comparisonMentioned)
  expect_true(evidence$uncertaintyMentioned)
  expect_true(evidence$noClearDifferenceMentioned)
})

test_that("semantic evidence recognises SG-4 question and model mismatch", {
  fixtures = readDeveloperScoringFixtures()
  fixture = fixtures[["test-SG-4"]]
  explanation = fixture$repeated$runs[[2]]$explanation

  evidence = extractWmfmSemanticEvidence(explanation, fixture$appState)

  expect_equal(evidence$effectDirection, "positive")
  expect_equal(evidence$effectScale, "multiplicative")
  expect_true(evidence$modelCannotAnswerQuestion)
  expect_false(evidence$researchQuestionAnsweredDirectly)
  expect_true(evidence$alternativeModelInterpretationProvided)
})

test_that("semantic evidence diagnostics expose stable fields without scoring marks", {
  fixtures = readDeveloperScoringFixtures()
  fixture = fixtures[["test-SG-4"]]
  explanation = fixture$repeated$runs[[2]]$explanation

  evidence = extractWmfmSemanticEvidence(explanation, fixture$appState)
  diagnostics = buildWmfmSemanticEvidenceDiagnostics(evidence)

  expect_s3_class(diagnostics, "data.frame")
  expect_named(
    diagnostics,
    c("field", "label", "value", "evidencePresent", "detail")
  )
  expect_true("modelCannotAnswerQuestion" %in% diagnostics$field)
  expect_equal(
    diagnostics$value[diagnostics$field == "modelCannotAnswerQuestion"],
    "present"
  )
  expect_equal(
    diagnostics$value[diagnostics$field == "effectDirection"],
    "positive"
  )
})


test_that("semantic evidence repairs missing deterministic claims before metric scoring", {
  fixtures = readDeveloperScoringFixtures()
  sg3 = fixtures[["test-SG-3"]]
  sg4 = fixtures[["test-SG-4"]]

  rawRecords = data.frame(
    explanationText = c(
      sg3$repeated$runs[[2]]$explanation,
      sg4$repeated$runs[[2]]$explanation
    ),
    formula = c(sg3$appState$formula, sg4$appState$formula),
    researchQuestion = c(sg3$appState$researchQuestion, sg4$appState$researchQuestion),
    modelType = c("lm", "logistic"),
    hasFactorPredictors = c(TRUE, FALSE),
    hasInteractionTerms = c(FALSE, FALSE),
    comparisonLanguageMention = c(TRUE, FALSE),
    effectDirectionClaim = c("not_stated", "not_stated"),
    effectScaleClaim = c("not_stated", "not_stated"),
    expectedEffectDirection = c("increase", "increase"),
    expectedEffectScale = c("additive", "multiplicative"),
    referenceGroupMention = c(FALSE, FALSE),
    uncertaintyMention = c(TRUE, TRUE),
    ciMention = c(TRUE, TRUE),
    usesInferentialLanguage = c(TRUE, TRUE),
    usesDescriptiveOnlyLanguage = c(FALSE, FALSE),
    overclaimDetected = c(FALSE, FALSE),
    underclaimDetected = c(FALSE, FALSE),
    conditionalLanguageMention = c(FALSE, FALSE),
    outcomeMention = c(TRUE, TRUE),
    predictorMention = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  scored = scoreWmfmRunRecordsCore(rawRecords, penaliseDuplicates = FALSE)

  expect_equal(scored$effectDirectionClaimAdjusted, c("increase", "increase"))
  expect_equal(scored$effectScaleClaimAdjusted, c("additive", "multiplicative"))
  expect_equal(scored$effectDirectionCorrect, c(2L, 2L))
  expect_equal(scored$effectScaleAppropriate, c(2L, 2L))
  expect_equal(scored$mainEffectCoverageAdequate, c(2L, 2L))
  expect_false(any(scored$fatalFlawDetected))
})
