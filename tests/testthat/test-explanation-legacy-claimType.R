testthat::test_that("getLegacyExplanationClaimTypePriority exposes the compatibility precedence", {
  priority = getLegacyExplanationClaimTypePriority()

  testthat::expect_identical(
    names(priority),
    c("researchQuestion", "answer", "typicalCase", "effect", "comparison", "uncertainty", "scale")
  )
  testthat::expect_identical(
    unname(priority),
    c("researchQuestion", "answer", "baseline", "mainEffect", "comparison", "uncertainty", "scale")
  )
})


testthat::test_that("deriveLegacyExplanationClaimType uses explicit priority for multi-tag sentences", {
  testthat::expect_identical(
    deriveLegacyExplanationClaimType(c("effect", "comparison", "answer")),
    "answer"
  )
  testthat::expect_identical(
    deriveLegacyExplanationClaimType(c("typicalCase", "effect", "uncertainty")),
    "baseline"
  )
  testthat::expect_identical(
    deriveLegacyExplanationClaimType(c("comparison", "uncertainty", "scale")),
    "comparison"
  )
  testthat::expect_identical(
    deriveLegacyExplanationClaimType(c("uncertainty", "scale")),
    "uncertainty"
  )
  testthat::expect_identical(
    deriveLegacyExplanationClaimType(c("researchQuestion", "answer", "effect")),
    "researchQuestion"
  )
  testthat::expect_identical(
    deriveLegacyExplanationClaimType(character(0)),
    "general"
  )
})


testthat::test_that("classifyExplanationClaimType treats claimTags as the compatibility source of truth", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_research_question") = "Does Test help explain Exam?"
  attr(model, "wmfm_response_noun_phrase") = "exam mark"

  audit = buildModelExplanationAudit(model)
  teachingSummary = buildExplanationTeachingSummary(audit = audit, model = model)

  claimType = classifyExplanationClaimType(
    claimText = "On average, exam marks tend to increase as Test increases.",
    audit = audit,
    teachingSummary = teachingSummary,
    model = model,
    sentenceIndex = 2,
    totalClaims = 2,
    claimTags = c("effect", "answer")
  )

  testthat::expect_identical(claimType, "answer")
})
