testthat::test_that("detectExplanationClaimTags keeps answer off earlier effect sentences", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_research_question") = "Does Test help explain Exam?"
  attr(model, "wmfm_response_noun_phrase") = "exam mark"

  audit = buildModelExplanationAudit(model)
  teachingSummary = buildExplanationTeachingSummary(audit = audit, model = model)

  tags = detectExplanationClaimTags(
    claimText = "On average, exam marks tend to increase as Test increases.",
    audit = audit,
    teachingSummary = teachingSummary,
    model = model,
    sentenceIndex = 1,
    totalClaims = 2
  )

  testthat::expect_identical(tags, "effect")
})


testthat::test_that("detectExplanationClaimTags can combine answer with comparison in the final sentence", {
  data(quakes, package = "datasets")

  df = datasets::quakes[, c("mag", "stations")]
  df$locn = factor(ifelse(seq_len(nrow(df)) %% 2 == 0, "SC", "WA"))

  model = stats::glm(stations ~ mag * locn, family = poisson(link = "log"), data = df)
  attr(model, "wmfm_research_question") = paste(
    "The study asks how the expected number of earthquakes changes when magnitude grows,",
    "and whether that pattern is the same for SC and WA."
  )

  audit = buildModelExplanationAudit(model)
  teachingSummary = buildExplanationTeachingSummary(audit = audit, model = model)

  tags = detectExplanationClaimTags(
    claimText = "On average, earthquake frequency falls as magnitude increases, with a steeper decline in WA than in SC.",
    audit = audit,
    teachingSummary = teachingSummary,
    model = model,
    sentenceIndex = 1,
    totalClaims = 1
  )

  testthat::expect_identical(tags, c("effect", "comparison", "answer"))
})


testthat::test_that("detectExplanationClaimTags can combine answer with uncertainty when the closing sentence is explicit", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_research_question") = "Does Test help explain Exam?"
  attr(model, "wmfm_response_noun_phrase") = "exam mark"

  audit = buildModelExplanationAudit(model)
  teachingSummary = buildExplanationTeachingSummary(audit = audit, model = model)

  tags = detectExplanationClaimTags(
    claimText = paste(
      "To answer the research question, exam marks tend to increase as Test increases,",
      "although the confidence interval still shows uncertainty."
    ),
    audit = audit,
    teachingSummary = teachingSummary,
    model = model,
    sentenceIndex = 2,
    totalClaims = 2
  )

  testthat::expect_identical(tags, c("effect", "uncertainty", "answer"))
})


testthat::test_that("buildExplanationClaimEvidenceMap keeps answer on a one-sentence closing takeaway", {
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

  testthat::expect_equal(nrow(out$claims), 1)
  testthat::expect_identical(out$claims$claimTags[[1]], c("effect", "answer"))
  testthat::expect_identical(out$claims$claimType[[1]], "answer")
})


testthat::test_that("buildExplanationClaimEvidenceMap keeps comparison and answer together in a final closing takeaway", {
  data(quakes, package = "datasets")

  df = datasets::quakes[, c("mag", "stations")]
  df$locn = factor(ifelse(seq_len(nrow(df)) %% 2 == 0, "SC", "WA"))

  model = stats::glm(stations ~ mag * locn, family = poisson(link = "log"), data = df)
  attr(model, "wmfm_research_question") = paste(
    "The study asks how the expected number of earthquakes changes when magnitude grows,",
    "and whether that pattern is the same for SC and WA."
  )

  audit = buildModelExplanationAudit(model)
  teachingSummary = buildExplanationTeachingSummary(audit = audit, model = model)

  out = buildExplanationClaimEvidenceMap(
    explanationText = paste(
      "In SC, the expected count falls as magnitude increases.",
      "On average, earthquake frequency falls as magnitude increases, with a steeper decline in WA than in SC."
    ),
    audit = audit,
    teachingSummary = teachingSummary,
    model = model
  )

  testthat::expect_identical(out$claims$claimTags[[1]], "effect")
  testthat::expect_identical(out$claims$claimTags[[2]], c("effect", "comparison", "answer"))
  testthat::expect_identical(out$claims$claimType[[2]], "answer")
})
