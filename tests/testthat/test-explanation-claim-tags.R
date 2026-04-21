testthat::test_that("detectExplanationClaimTags returns overlapping tags in a stable order", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_research_question") = "Does Test help explain Exam?"
  attr(model, "wmfm_response_noun_phrase") = "exam mark"

  audit = buildModelExplanationAudit(model)
  teachingSummary = buildExplanationTeachingSummary(
    audit = audit,
    model = model,
    researchQuestion = "Does Test help explain Exam?"
  )

  anchorValue = buildExplanationTeachingNumber(audit$numericAnchor$table$anchor[[1]])

  tags = detectExplanationClaimTags(
    claimText = paste0(
      "When Test is around ",
      anchorValue,
      ", exam marks tend to increase, but the confidence interval shows some uncertainty."
    ),
    audit = audit,
    teachingSummary = teachingSummary,
    model = model,
    sentenceIndex = 2,
    totalClaims = 3
  )

  testthat::expect_identical(tags, c("typicalCase", "effect", "uncertainty"))
})
