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


testthat::test_that("interaction answer selection keeps answer off intermediate slope statements", {
  data(course.df, package = "s20x")
  df = course.df[, c("Exam", "Attend", "Test")]

  model = stats::lm(Exam ~ Attend * Test, data = df)
  attr(model, "wmfm_research_question") = paste(
    "Does the relationship between test mark and final exam mark appear to differ",
    "by attendance group?"
  )
  attr(model, "wmfm_response_noun_phrase") = "exam mark"

  audit = buildModelExplanationAudit(model)
  teachingSummary = buildExplanationTeachingSummary(audit = audit, model = model)

  out = buildExplanationClaimEvidenceMap(
    explanationText = paste(
      "The question asks whether the link between a student's test mark and their final exam mark is different for those who attend class regularly compared to those who do not.",
      "In this model, the effect of test mark on exam mark was estimated separately for the two attendance groups.",
      "For students who regularly attended class, the slope linking test to exam was steeper, indicating that each additional point on the test is associated with a larger increase in the final exam mark.",
      "For students who did not attend class, the slope was flatter, meaning that increases in test score produce smaller gains in the final exam.",
      "The comparison of these two slopes shows that the relationship is stronger for regular attendees.",
      "Overall, the effect of test mark on exam mark is not the same across attendance groups."
    ),
    audit = audit,
    teachingSummary = teachingSummary,
    model = model
  )

  intermediateRows = grep(
    "^(For students who regularly attended class|The comparison of these two slopes)",
    out$claims$claimText
  )
  answerRows = grep("^Overall,", out$claims$claimText)

  testthat::expect_length(intermediateRows, 2)
  testthat::expect_length(answerRows, 1)
  testthat::expect_false(any(vapply(out$claims$claimTags[intermediateRows], function(tags) {
    "answer" %in% tags
  }, logical(1))))
  testthat::expect_true("answer" %in% out$claims$claimTags[[answerRows]])
  testthat::expect_identical(out$claims$primaryRole[[answerRows]], "answer")
})
