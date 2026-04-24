testthat::test_that("sentence role schema maps legacy tags into the Stage 9 ontology", {
  roles = mapExplanationClaimTagsToSentenceRoles(
    claimTags = c("typicalCase", "effect", "uncertainty", "scale"),
    matchedEvidence = data.frame(
      evidenceId = "evidence_1",
      evidenceType = "effectEvidence",
      stringsAsFactors = FALSE
    ),
    claimText = "At x = 10, the fitted value is about 6.8."
  )

  testthat::expect_identical(
    roles,
    c("effect", "uncertainty", "typicalCase", "evidence", "scaleTranslation")
  )
})


testthat::test_that("sentence role schema derives a stable primary role", {
  testthat::expect_identical(
    deriveExplanationPrimaryRole(c("evidence", "answer", "uncertainty")),
    "answer"
  )

  testthat::expect_identical(
    deriveExplanationPrimaryRole(character(0)),
    "modelContext"
  )
})


testthat::test_that("sentence role schema records support-map ids", {
  matchedEvidence = data.frame(
    evidenceId = c("evidence_2", "evidence_2", "evidence_3"),
    evidenceType = c("effectEvidence", "effectEvidence", "confidenceInterval"),
    stringsAsFactors = FALSE
  )

  testthat::expect_identical(
    buildExplanationSentenceSupportMapIds(matchedEvidence),
    c("evidence_2", "evidence_3")
  )
})


testthat::test_that("sentence detectors recognise model constraints and statistical disclaimers", {
  roles = detectExplanationSentenceRoles(
    claimText = paste(
      "These results describe statistical associations, not causal relationships,",
      "and only apply on average rather than to a particular case."
    ),
    claimTags = character(0)
  )

  testthat::expect_true("statisticalDisclaimer" %in% roles)

  roles = detectExplanationSentenceRoles(
    claimText = "Because this model has no predictors, it does not compare groups.",
    claimTags = character(0)
  )

  testthat::expect_true("modelConstraint" %in% roles)
})

testthat::test_that("claim evidence map exposes Stage 9 schema fields without dropping legacy fields", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]
  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_research_question") = "Does Test help explain Exam?"
  attr(model, "wmfm_response_noun_phrase") = "exam mark"

  audit = buildModelExplanationAudit(model)
  teachingSummary = buildExplanationTeachingSummary(audit = audit, model = model)

  out = buildExplanationClaimEvidenceMap(
    explanationText = "On average, exam marks tend to increase as Test increases.",
    audit = audit,
    teachingSummary = teachingSummary,
    model = model
  )

  testthat::expect_true(all(c(
    "claimTags",
    "claimType",
    "primaryRole",
    "roles",
    "qualityFlags",
    "supportMapIds"
  ) %in% names(out$claims)))

  testthat::expect_true("effect" %in% out$claims$roles[[1]])
  testthat::expect_identical(out$claims$primaryRole[[1]], "effect")
  testthat::expect_true(is.character(out$claims$qualityFlags[[1]]))
  testthat::expect_true(is.character(out$claims$supportMapIds[[1]]))
})
