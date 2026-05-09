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

test_that("semantic evidence repairs comparison, uncertainty, and interaction mentions", {
  fixtures = readDeveloperScoringFixtures()
  sg3 = fixtures[["test-SG-3"]]
  sg5 = fixtures[["test-SG-5"]]

  rawRecords = data.frame(
    explanationText = c(
      sg3$repeated$runs[[2]]$explanation,
      sg5$repeated$runs[[1]]$explanation
    ),
    formula = c(sg3$appState$formula, sg5$appState$formula),
    researchQuestion = c(sg3$appState$researchQuestion, sg5$appState$researchQuestion),
    modelType = c("lm", "lm"),
    hasFactorPredictors = c(TRUE, TRUE),
    hasInteractionTerms = c(FALSE, TRUE),
    comparisonLanguageMention = c(FALSE, FALSE),
    interactionMention = c(FALSE, FALSE),
    effectDirectionClaim = c("not_stated", "increase"),
    effectScaleClaim = c("not_stated", "additive"),
    interactionSubstantiveClaim = c("not_applicable", "no_clear_difference"),
    expectedEffectDirection = c("increase", "increase"),
    expectedEffectScale = c("additive", "additive"),
    referenceGroupMention = c(FALSE, FALSE),
    uncertaintyMention = c(FALSE, FALSE),
    ciMention = c(FALSE, FALSE),
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

  expect_true(all(scored$semanticComparisonMentioned))
  expect_true(all(scored$semanticUncertaintyMentioned))
  expect_true(scored$semanticInteractionAcknowledged[2])

  expect_equal(scored$referenceGroupHandledCorrectly[1], 2L)
  expect_equal(scored$referenceGroupCoverageAdequate[1], 2L)
  expect_equal(scored$comparisonStructureClear[1], 2L)
  expect_equal(scored$uncertaintyHandlingAppropriate, c(2L, 2L))
  expect_equal(scored$interactionCoverageAdequate[2], 2L)
})

test_that("semantic evidence repairs question-model mismatch coverage", {
  fixtures = readDeveloperScoringFixtures()
  sg4 = fixtures[["test-SG-4"]]

  rawRecords = data.frame(
    explanationText = sg4$repeated$runs[[2]]$explanation,
    formula = sg4$appState$formula,
    researchQuestion = sg4$appState$researchQuestion,
    modelType = "logistic",
    hasFactorPredictors = FALSE,
    hasInteractionTerms = FALSE,
    comparisonLanguageMention = FALSE,
    effectDirectionClaim = "not_stated",
    effectScaleClaim = "not_stated",
    expectedEffectDirection = "increase",
    expectedEffectScale = "multiplicative",
    referenceGroupMention = FALSE,
    uncertaintyMention = FALSE,
    ciMention = TRUE,
    usesInferentialLanguage = TRUE,
    usesDescriptiveOnlyLanguage = FALSE,
    overclaimDetected = FALSE,
    underclaimDetected = FALSE,
    conditionalLanguageMention = FALSE,
    outcomeMention = FALSE,
    predictorMention = FALSE,
    stringsAsFactors = FALSE
  )

  scored = scoreWmfmRunRecordsCore(rawRecords, penaliseDuplicates = FALSE)

  expect_true(scored$semanticModelCannotAnswerQuestion)
  expect_false(scored$semanticResearchQuestionAnsweredDirectly)
  expect_true(scored$semanticAlternativeModelInterpretationProvided)
  expect_true(scored$semanticModelMismatchExplained)
  expect_true(scored$outcomeMentionAdjusted)
  expect_true(scored$predictorMentionAdjusted)
  expect_equal(scored$clarityScore, 2)
  expect_false(scored$fatalFlawDetected)
})

test_that("semantic evidence treats no-clear-interaction explanations as substantively correct", {
  fixtures = readDeveloperScoringFixtures()
  sg5 = fixtures[["test-SG-5"]]

  rawRecords = data.frame(
    explanationText = sg5$repeated$runs[[3]]$explanation,
    formula = sg5$appState$formula,
    researchQuestion = sg5$appState$researchQuestion,
    modelType = "lm",
    hasFactorPredictors = TRUE,
    hasInteractionTerms = TRUE,
    interactionMinPValue = 0.40,
    interactionAlpha = 0.05,
    comparisonLanguageMention = FALSE,
    interactionMention = FALSE,
    effectDirectionClaim = "increase",
    effectScaleClaim = "additive",
    interactionSubstantiveClaim = "no_clear_difference",
    expectedEffectDirection = "increase",
    expectedEffectScale = "additive",
    referenceGroupMention = FALSE,
    uncertaintyMention = FALSE,
    ciMention = FALSE,
    usesInferentialLanguage = TRUE,
    usesDescriptiveOnlyLanguage = FALSE,
    overclaimDetected = FALSE,
    underclaimDetected = FALSE,
    conditionalLanguageMention = FALSE,
    outcomeMention = TRUE,
    predictorMention = TRUE,
    stringsAsFactors = FALSE
  )

  scored = scoreWmfmRunRecordsCore(rawRecords, penaliseDuplicates = FALSE)

  expect_true(scored$semanticInteractionAcknowledged)
  expect_true(scored$semanticNoClearDifferenceMentioned)
  expect_equal(scored$interactionSubstantiveCorrect, 2L)
  expect_equal(scored$interactionCoverageAdequate, 2L)
  expect_false(scored$fatalFlawDetected)
})

test_that("semantic evidence recognises no-change interaction uncertainty", {
  fixtures = readDeveloperScoringFixtures()
  sg5 = fixtures[["test-SG-5"]]

  explanation = sg5$current$explanation
  evidence = extractWmfmSemanticEvidence(explanation, sg5$appState)

  expect_true(evidence$interactionAcknowledged)
  expect_true(evidence$uncertaintyMentioned)
  expect_true(evidence$noClearDifferenceMentioned)

  rawRecords = data.frame(
    explanationText = explanation,
    formula = sg5$appState$formula,
    researchQuestion = sg5$appState$researchQuestion,
    modelType = "lm",
    hasFactorPredictors = TRUE,
    hasInteractionTerms = TRUE,
    interactionMinPValue = 0.40,
    interactionAlpha = 0.05,
    comparisonLanguageMention = FALSE,
    interactionMention = FALSE,
    effectDirectionClaim = "not_stated",
    effectScaleClaim = "not_stated",
    interactionSubstantiveClaim = "unclear",
    expectedEffectDirection = "increase",
    expectedEffectScale = "additive",
    referenceGroupMention = FALSE,
    uncertaintyMention = FALSE,
    ciMention = FALSE,
    usesInferentialLanguage = TRUE,
    usesDescriptiveOnlyLanguage = FALSE,
    overclaimDetected = FALSE,
    underclaimDetected = FALSE,
    conditionalLanguageMention = FALSE,
    outcomeMention = TRUE,
    predictorMention = TRUE,
    stringsAsFactors = FALSE
  )

  scored = scoreWmfmRunRecordsCore(rawRecords, penaliseDuplicates = FALSE)

  expect_true(scored$semanticInteractionAcknowledged)
  expect_true(scored$semanticNoClearDifferenceMentioned)
  expect_equal(scored$interactionSubstantiveCorrect, 2L)
  expect_equal(scored$interactionCoverageAdequate, 2L)
})

test_that("semantic evidence recognises unsupported-difference wording", {
  modelInfo = list(
    formula = "Exam ~ Attend * Gender + Test",
    researchQuestion = "Does the relationship between attendance and final exam mark differ by gender after accounting for the mid-term test mark?",
    modelType = "lm",
    hasFactorPredictors = TRUE,
    hasInteractionTerms = TRUE
  )
  explanation = paste(
    "The model includes the attendance by gender interaction.",
    "Attendance is positively associated with exam marks for both groups,",
    "but the data do not support a gender difference in the attendance effect.",
    "There is insufficient evidence to confirm that one gender benefits more from attendance."
  )

  evidence = extractWmfmSemanticEvidence(explanation, modelInfo)

  expect_true(evidence$interactionAcknowledged)
  expect_true(evidence$uncertaintyMentioned)
  expect_true(evidence$noClearDifferenceMentioned)
})

test_that("semantic evidence recognises cautious no-evidence interaction wording", {
  modelInfo = list(
    formula = "Exam ~ Attend * Gender + Test",
    researchQuestion = "Does the relationship between attendance and final exam mark differ by gender after accounting for the mid-term test mark?",
    modelType = "lm",
    hasFactorPredictors = TRUE,
    hasInteractionTerms = TRUE
  )
  explanation = paste(
    "The fitted model includes the attendance by gender interaction.",
    "Attendance is positively associated with final exam marks for both groups.",
    "There is no evidence that the attendance relationship differs by gender,",
    "so we cannot conclude that one group benefits more than the other."
  )

  evidence = extractWmfmSemanticEvidence(explanation, modelInfo)

  expect_true(evidence$interactionAcknowledged)
  expect_true(evidence$uncertaintyMentioned)
  expect_true(evidence$noClearDifferenceMentioned)
})


test_that("semantic evidence recognises unlikely interaction wording", {
  evidence = extractWmfmSemanticEvidence(
    "The interaction is unlikely and the data do not support a meaningful difference."
  )

  expect_true(isTRUE(evidence$uncertaintyPresent))
  expect_true(isTRUE(evidence$noClearDifference))
})

test_that("semantic evidence recognises consolidated cautious interaction wording", {
  cautiousPhrases = c(
    "The interaction is compatible with no difference between groups.",
    "The fitted interaction is not convincing evidence of a gender difference.",
    "The data are not strong enough to confirm a different attendance effect.",
    "We cannot distinguish the attendance slopes clearly from this model."
  )

  for (phrase in cautiousPhrases) {
    evidence = extractWmfmSemanticEvidence(phrase)

    expect_true(
      isTRUE(evidence$uncertaintyPresent),
      info = phrase
    )
    expect_true(
      isTRUE(evidence$noClearDifference),
      info = phrase
    )
  }
})

test_that("semantic cautious wording patterns are maintained centrally", {
  uncertaintyPatterns = wmfmSemanticUncertaintyPatterns()
  noClearDifferencePatterns = wmfmSemanticNoClearDifferencePatterns()

  expect_true("unlikely" %in% uncertaintyPatterns)
  expect_true("unlikely" %in% noClearDifferencePatterns)
  expect_true("compatible with no (change|difference)" %in% uncertaintyPatterns)
  expect_true("compatible with no (change|difference)" %in% noClearDifferencePatterns)
  expect_true("not strong enough" %in% uncertaintyPatterns)
  expect_true("not strong enough" %in% noClearDifferencePatterns)
})

