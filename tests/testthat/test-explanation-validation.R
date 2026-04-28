testthat::test_that("deterministic sentence validation flags technical scale leakage", {
  flags = buildExplanationDeterministicQualityFlags(
    claimText = "The log-odds coefficient is 1.23456.",
    roles = c("effect", "evidence")
  )

  testthat::expect_true("technicalScaleLeakage" %in% flags)
  testthat::expect_true("rawCoefficientShown" %in% flags)
  testthat::expect_true("excessiveDecimalPlaces" %in% flags)
})


testthat::test_that("deterministic sentence validation flags odds shown as a decimal", {
  flags = buildExplanationDeterministicQualityFlags(
    claimText = "The odds of passing are about 0.70.",
    roles = c("effect", "evidence")
  )

  testthat::expect_true("oddsShownAsDecimal" %in% flags)

  flags = buildExplanationDeterministicQualityFlags(
    claimText = "The odds of passing are about 0.70:1.",
    roles = c("effect", "evidence")
  )

  testthat::expect_false("oddsShownAsDecimal" %in% flags)
})



testthat::test_that("deterministic sentence validation does not flag odds-ratio intervals as decimal odds", {
  flags = buildExplanationDeterministicQualityFlags(
    claimText = "The odds ratio for attendance is 2.5 (95% CI 0.8 to 7.7), indicating that the evidence for a true difference is weak because the interval includes no change.",
    roles = c("uncertainty", "evidence")
  )

  testthat::expect_false("oddsShownAsDecimal" %in% flags)

  flags = buildExplanationDeterministicQualityFlags(
    claimText = "The odds of passing are about 0.70 (95% CI 0.4 to 1.2).",
    roles = c("effect", "uncertainty", "evidence")
  )

  testthat::expect_true("oddsShownAsDecimal" %in% flags)
})

testthat::test_that("deterministic sentence validation flags vague numeric effect language", {
  flags = buildExplanationDeterministicQualityFlags(
    claimText = "The effect of magnitude is negative.",
    roles = "effect"
  )

  testthat::expect_true("effectWithoutChangeLanguage" %in% flags)

  flags = buildExplanationDeterministicQualityFlags(
    claimText = "The effect of a one-unit increase in magnitude is negative.",
    roles = "effect"
  )

  testthat::expect_false("effectWithoutChangeLanguage" %in% flags)
})


testthat::test_that("claim evidence map exposes deterministic map-level quality flags", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]
  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_research_question") = "Does Test help explain Exam?"
  attr(model, "wmfm_response_noun_phrase") = "exam mark"

  audit = buildModelExplanationAudit(model)
  teachingSummary = buildExplanationTeachingSummary(audit = audit, model = model)

  out = buildExplanationClaimEvidenceMap(
    explanationText = "The log-odds coefficient is 1.23456.",
    audit = audit,
    teachingSummary = teachingSummary,
    model = model
  )

  testthat::expect_false("qualityFlags" %in% names(out))
  testthat::expect_true("technicalScaleLeakage" %in% attr(out, "qualityFlags"))
  testthat::expect_true("rawCoefficientShown" %in% attr(out, "qualityFlags"))
  testthat::expect_true("technicalScaleLeakage" %in% out$claims$qualityFlags[[1]])
})
