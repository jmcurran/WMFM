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


testthat::test_that("detectExplanationClaimTags handles effect plus uncertainty plus comparison", {
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
    claimText = paste(
      "Compared with SC, WA shows a steeper decline,",
      "but the confidence limits still show uncertainty around that difference."
    ),
    audit = audit,
    teachingSummary = teachingSummary,
    model = model,
    sentenceIndex = 2,
    totalClaims = 3
  )

  testthat::expect_identical(tags, c("effect", "uncertainty", "comparison"))
})

testthat::test_that("detectExplanationClaimTags keeps scale-only sentences separate from effect tags", {
  tags = detectExplanationClaimTags(
    claimText = "This result is shown on the probability scale rather than the odds scale.",
    audit = list(),
    teachingSummary = NULL,
    model = NULL,
    sentenceIndex = 1,
    totalClaims = 1
  )

  testthat::expect_identical(tags, "scale")
})
