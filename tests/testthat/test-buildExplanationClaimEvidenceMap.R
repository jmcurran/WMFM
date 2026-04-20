testthat::test_that("buildExplanationClaimEvidenceMap returns sentence-level deterministic mappings", {
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
  baselineLabel = audit$baselineEvidence$quantity[[1]]

  explanationText = paste(
    "Does Test help explain Exam?",
    "On average, exam marks tend to increase as Test increases.",
    paste0(
      "When Test is around ",
      anchorValue,
      ", the fitted value is summarised by ",
      baselineLabel,
      "."
    ),
    "These confidence intervals suggest the increase is likely to be positive."
  )

  out = buildExplanationClaimEvidenceMap(
    explanationText = explanationText,
    audit = audit,
    teachingSummary = teachingSummary,
    model = model
  )

  testthat::expect_s3_class(out, "wmfmExplanationClaimEvidenceMap")
  testthat::expect_named(
    out,
    c("transparencyNote", "mappingMethod", "unit", "evidenceInventory", "claims")
  )
  testthat::expect_identical(out$unit, "sentence")
  testthat::expect_true(is.data.frame(out$evidenceInventory))
  testthat::expect_true(is.data.frame(out$claims))
  testthat::expect_equal(nrow(out$claims), 4)
  testthat::expect_true(all(c(
    "claimId",
    "sentenceIndex",
    "claimText",
    "claimType",
    "supportNote",
    "evidenceCount",
    "evidenceTypes",
    "evidenceLabels",
    "mappingMethod"
  ) %in% names(out$claims)))
  testthat::expect_match(out$transparencyNote, "does not claim to reveal hidden chain-of-thought", fixed = TRUE)
  testthat::expect_true(all(out$claims$evidenceCount >= 1))
  testthat::expect_identical(out$claims$claimType[[1]], "researchQuestion")
  testthat::expect_identical(out$claims$claimType[[2]], "mainEffect")
  testthat::expect_identical(out$claims$claimType[[3]], "baseline")
  testthat::expect_identical(out$claims$claimType[[4]], "uncertainty")
  testthat::expect_match(out$claims$evidenceLabels[[3]], baselineLabel, fixed = TRUE)
  testthat::expect_match(out$claims$supportNote[[4]], "confidence-interval guidance", fixed = TRUE)
})


testthat::test_that("buildExplanationClaimEvidenceMap handles factor comparisons", {
  data(quakes, package = "datasets")

  df = datasets::quakes[, c("mag", "stations")]
  df$highMag = factor(ifelse(df$mag >= median(df$mag), "high", "low"))

  model = stats::lm(stations ~ highMag, data = df)
  attr(model, "wmfm_response_noun_phrase") = "number of stations"

  audit = buildModelExplanationAudit(model)
  teachingSummary = buildExplanationTeachingSummary(audit = audit, model = model)

  referenceLevel = audit$referenceLevels$referenceLevel[[1]]

  explanationText = paste(
    "The fitted model compares the highMag groups.",
    paste0("The reference group is ", referenceLevel, ", so the other group is described relative to it.")
  )

  out = buildExplanationClaimEvidenceMap(
    explanationText = explanationText,
    audit = audit,
    teachingSummary = teachingSummary,
    model = model
  )

  testthat::expect_equal(nrow(out$claims), 2)
  testthat::expect_identical(out$claims$claimType[[2]], "comparison")
  testthat::expect_match(out$claims$evidenceLabels[[2]], "Reference level", fixed = TRUE)
})


testthat::test_that("buildExplanationClaimEvidenceMap treats research-question paraphrases as framing", {
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

  out = buildExplanationClaimEvidenceMap(
    explanationText = paste(
      "The study asks whether Test helps explain Exam.",
      "On average, exam marks tend to increase as Test increases."
    ),
    audit = audit,
    teachingSummary = teachingSummary,
    model = model
  )

  testthat::expect_identical(out$claims$claimType[[1]], "researchQuestion")
  testthat::expect_identical(out$claims$evidenceLabels[[1]], "Research question framing")
})


testthat::test_that("effect sentences with intervals are not treated as baseline cases", {
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
      "A one-magnitude rise multiplies the expected count in SC by about 0.21; the 95% confidence limits run from roughly 0.13 to 0.31.",
      "In WA the same one-magnitude rise multiplies the expected count by about 0.04, with limits from roughly 0.04 to 0.72.",
      "Answer: On average, earthquake frequency falls as magnitude increases, with a steeper decline in WA."
    ),
    audit = audit,
    teachingSummary = teachingSummary,
    model = model
  )

  testthat::expect_identical(out$claims$claimType[[1]], "mainEffect")
  testthat::expect_identical(out$claims$claimType[[2]], "mainEffect")
  testthat::expect_match(out$claims$supportNote[[1]], "response changes", fixed = TRUE)
  testthat::expect_match(out$claims$supportNote[[2]], "response changes", fixed = TRUE)
  testthat::expect_identical(out$claims$claimType[[3]], "answer")
  testthat::expect_match(out$claims$supportNote[[3]], "overall answer", fixed = TRUE)
})
