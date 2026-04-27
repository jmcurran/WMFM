testthat::test_that("buildExplanationClaimEvidenceMap keeps answer on the last substantive sentence before a disclaimer", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_research_question") = "Does Test help explain Exam?"
  attr(model, "wmfm_response_noun_phrase") = "exam mark"

  audit = buildModelExplanationAudit(model)
  teachingSummary = buildExplanationTeachingSummary(audit = audit, model = model)

  out = buildExplanationClaimEvidenceMap(
    explanationText = paste(
      "Does Test help explain Exam?",
      "Overall, exam marks tend to increase as Test increases.",
      "This association reflects average effects and does not guarantee individual outcomes."
    ),
    audit = audit,
    teachingSummary = teachingSummary,
    model = model
  )

  testthat::expect_identical(out$claims$claimTags[[1]], "researchQuestion")
  testthat::expect_identical(out$claims$claimTags[[2]], c("effect", "answer"))
  testthat::expect_identical(out$claims$claimType[[2]], "answer")
  testthat::expect_false("answer" %in% out$claims$claimTags[[3]])
})


testthat::test_that("buildExplanationClaimEvidenceMap can place answer on an in-short summary before a trailing caution", {
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
      "These figures indicate that higher magnitudes are associated with fewer earthquakes in both regions, with a stronger decrease in WA than in SC.",
      "In short, on average earthquake frequency falls sharply as magnitude increases in both locations, with WA showing an even steeper decrease than SC, according to the fitted model and its uncertainty.",
      "This pattern applies on average and does not predict individual earthquakes."
    ),
    audit = audit,
    teachingSummary = teachingSummary,
    model = model
  )

  testthat::expect_identical(out$claims$claimTags[[2]], c("effect", "uncertainty", "comparison", "answer"))
  testthat::expect_identical(out$claims$claimType[[2]], "answer")
  testthat::expect_false("answer" %in% out$claims$claimTags[[3]])
})


testthat::test_that("buildExplanationClaimEvidenceMap uses context evidence for untagged setup sentences", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_response_noun_phrase") = "exam mark"

  audit = buildModelExplanationAudit(model)
  teachingSummary = buildExplanationTeachingSummary(audit = audit, model = model)

  out = buildExplanationClaimEvidenceMap(
    explanationText = teachingSummary$dataDescription,
    audit = audit,
    teachingSummary = teachingSummary,
    model = model
  )

  testthat::expect_gte(nrow(out$claims), 2)
  testthat::expect_true(all(vapply(out$claims$claimTags, length, integer(1)) == 0L))
  testthat::expect_true(any(grepl("Data description", out$claims$evidenceLabels[[1]], fixed = TRUE)))
  testthat::expect_false(any(grepl("Main effect translation", out$claims$evidenceLabels[[1]], fixed = TRUE)))
  testthat::expect_false(any(grepl("Confidence interval rule", out$claims$evidenceLabels[[1]], fixed = TRUE)))
})
