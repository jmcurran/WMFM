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

