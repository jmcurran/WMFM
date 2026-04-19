test_that("buildModelExplanationAudit returns a deterministic audit for a linear model", {
  df = getStats20xExamTestData()

  model = stats::lm(Exam ~ Test + Gender, data = df)
  attr(model, "wmfm_research_question") = "How does Exam change with Test and Gender?"
  attr(model, "wmfm_dataset_doc") = "Exam: exam mark
Test: test mark
Gender: student gender"
  attr(model, "wmfm_dataset_name") = "STATS20x"
  attr(model, "wmfm_response_noun_phrase") = "exam mark"

  out = buildModelExplanationAudit(model)

  expect_s3_class(out, "wmfmExplanationAudit")
  expect_match(out$transparencyNote, "does not claim to reveal hidden chain-of-thought")
  expect_identical(out$promptInputs$researchQuestionUsed, TRUE)
  expect_true(is.data.frame(out$numericAnchor$table))
  expect_true("Test" %in% out$numericAnchor$table$predictor)
  expect_true(is.data.frame(out$referenceLevels))
  expect_true("Gender" %in% out$referenceLevels$predictor)
  expect_true(is.data.frame(out$baselineEvidence))
  expect_true(nrow(out$baselineEvidence) >= 1)
  expect_true(is.data.frame(out$coefficientTable))
  expect_true("term" %in% names(out$coefficientTable))
})

test_that("buildModelExplanationAudit reports odds-scale interpretation for logistic models", {
  df = data.frame(
    pass = factor(c("Fail", "Fail", "Pass", "Pass", "Pass", "Fail")),
    x = c(-2, -1, 0, 1, 2, 0.5)
  )

  model = stats::glm(
    pass ~ x,
    data = df,
    family = stats::binomial(link = "logit")
  )

  out = buildModelExplanationAudit(model)

  expect_identical(out$interpretationScale$effectScale, "odds multipliers")
  expect_match(out$interpretationScale$backTransformation, "inverse-logit", fixed = TRUE)
  expect_true(any(out$confidenceIntervals$displayedScales %in% c("probability", "odds", "odds multiplier")))
})
