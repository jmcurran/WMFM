test_that("buildModelExplanationAudit returns the required contract sections for a linear model", {
  df = getStats20xExamTestData()

  model = stats::lm(Exam ~ Test + Gender, data = df)
  attr(model, "wmfm_research_question") = "How does Exam change with Test and Gender?"
  attr(model, "wmfm_dataset_doc") = paste(
    "Exam: exam mark",
    "Test: test mark",
    "Gender: student gender",
    sep = "\n"
  )
  attr(model, "wmfm_dataset_name") = "STATS20x"
  attr(model, "wmfm_response_noun_phrase") = "exam mark"

  out = buildModelExplanationAudit(model)

  expect_s3_class(out, "wmfmExplanationAudit")
  expect_true(is.list(out))
  expect_true(all(c(
    "transparencyNote",
    "overview",
    "promptInputs",
    "promptRules",
    "interpretationScale",
    "numericAnchor",
    "referenceLevels",
    "confidenceIntervals",
    "baselineEvidence",
    "effectEvidence",
    "coefficientTable",
    "rawPromptIngredients"
  ) %in% names(out)))

  expect_match(out$transparencyNote, "deterministic inputs", fixed = TRUE)
  expect_match(
    out$transparencyNote,
    "does not claim to reveal hidden chain-of-thought",
    fixed = TRUE
  )

  expect_identical(out$overview$response, "Exam")
  expect_true(all(c("Test", "Gender") %in% out$overview$predictors))
  expect_identical(out$overview$modelFamily, "gaussian")
  expect_identical(out$overview$link, "identity")
  expect_true(is.numeric(out$overview$nObservations))
  expect_true(out$overview$nObservations >= 1)
  expect_identical(out$overview$hasDatasetContext, TRUE)
  expect_identical(out$overview$hasResearchQuestion, TRUE)

  expect_identical(out$promptInputs$response, "Exam")
  expect_identical(out$promptInputs$responseNounPhrase, "exam mark")
  expect_true(all(c("Test", "Gender") %in% out$promptInputs$predictors))
  expect_identical(out$promptInputs$datasetContextUsed, TRUE)
  expect_identical(out$promptInputs$datasetName, "STATS20x")
  expect_identical(out$promptInputs$researchQuestionUsed, TRUE)
  expect_identical(
    out$promptInputs$researchQuestion,
    "How does Exam change with Test and Gender?"
  )
  expect_identical(out$promptInputs$coefficientTableIncluded, TRUE)
  expect_identical(out$promptInputs$confidenceIntervalsIncluded, TRUE)
  expect_identical(out$promptInputs$precomputedBaselineValuesIncluded, TRUE)
  expect_identical(out$promptInputs$numericAnchorRuleIncluded, TRUE)
  expect_true(is.numeric(out$promptInputs$nObservations))
  expect_true(out$promptInputs$nObservations >= 1)

  expect_true(is.character(out$promptRules))
  expect_true(length(out$promptRules) >= 1)
  expect_true(any(grepl("plain language", out$promptRules, fixed = TRUE)))
  expect_true(any(grepl("numeric anchor", out$promptRules, fixed = TRUE)))

  expect_identical(out$interpretationScale$responseExpression, "Exam")
  expect_identical(out$interpretationScale$responseTransform, "none")
  expect_identical(out$interpretationScale$fittedValueScale, "response scale")
  expect_identical(
    out$interpretationScale$effectScale,
    "additive response-scale differences"
  )
  expect_match(
    out$interpretationScale$backTransformation,
    "No back-transformation is required.",
    fixed = TRUE
  )

  expect_true(out$numericAnchor$numericReference %in% c("zero", "mean"))
  expect_true(is.character(out$numericAnchor$note))
  expect_true(is.data.frame(out$numericAnchor$table))
  expect_true(all(c("predictor", "observedRange", "anchor", "reason") %in% names(out$numericAnchor$table)))
  expect_true("Test" %in% out$numericAnchor$table$predictor)

  expect_true(is.data.frame(out$referenceLevels))
  expect_true(all(c("predictor", "referenceLevel", "levels") %in% names(out$referenceLevels)))
  expect_true("Gender" %in% out$referenceLevels$predictor)

  expect_true(is.list(out$confidenceIntervals))
  expect_true(all(c(
    "level",
    "mode",
    "note",
    "teachingNote",
    "displayedScales"
  ) %in% names(out$confidenceIntervals)))
  expect_true(
    is.numeric(out$confidenceIntervals$level) ||
      is.na(out$confidenceIntervals$level)
  )
  expect_true(is.character(out$confidenceIntervals$displayedScales))

  expect_true(is.data.frame(out$baselineEvidence))
  expect_true(is.data.frame(out$effectEvidence))
  expect_true(is.data.frame(out$coefficientTable))
  expect_true(nrow(out$coefficientTable) >= 2)
  expect_true("term" %in% names(out$coefficientTable))
  expect_true(any(grepl("Intercept", out$coefficientTable$term, fixed = TRUE)))
  expect_true("Test" %in% out$coefficientTable$term)

  expect_true(is.list(out$rawPromptIngredients))
  expect_true(all(c(
    "languageContract",
    "numericAnchorPrompt",
    "anchoredBaselinePrompt"
  ) %in% names(out$rawPromptIngredients)))
  expect_true(all(vapply(out$rawPromptIngredients, is.character, logical(1))))
})

test_that("buildModelExplanationAudit uses sample-mean anchors when zero is outside the observed range", {
  df = data.frame(
    y = c(3, 4, 5, 6, 7),
    x = c(10, 12, 14, 16, 18)
  )

  model = stats::lm(y ~ x, data = df)
  out = buildModelExplanationAudit(model)

  expect_identical(out$numericAnchor$numericReference, "mean")
  expect_true(is.data.frame(out$numericAnchor$table))
  expect_identical(out$numericAnchor$table$predictor, "x")
  expect_equal(out$numericAnchor$table$anchor, mean(df$x))
  expect_true(any(grepl(
    "outside the observed range",
    out$numericAnchor$table$reason,
    fixed = TRUE
  )))
})

test_that("buildModelExplanationAudit handles factor-only models without numeric anchors", {
  df = data.frame(
    y = c(5, 6, 7, 8, 9, 10),
    grp = factor(c("A", "A", "B", "B", "C", "C"))
  )

  model = stats::lm(y ~ grp, data = df)
  out = buildModelExplanationAudit(model)

  expect_identical(out$numericAnchor$numericReference, "zero")
  expect_true(is.data.frame(out$numericAnchor$table))
  expect_identical(nrow(out$numericAnchor$table), 0L)
  expect_match(
    out$numericAnchor$note,
    "No numeric predictors were present",
    fixed = TRUE
  )
  expect_true(is.data.frame(out$referenceLevels))
  expect_identical(out$referenceLevels$predictor, "grp")
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

  expect_identical(out$overview$modelFamily, "binomial")
  expect_identical(out$overview$link, "logit")
  expect_identical(out$interpretationScale$fittedValueScale, "probability and odds")
  expect_identical(out$interpretationScale$effectScale, "odds multipliers")
  expect_match(out$interpretationScale$backTransformation, "inverse-logit", fixed = TRUE)
  expect_true(is.character(out$confidenceIntervals$displayedScales))
})

test_that("buildModelExplanationAudit reports expected-count interpretation for Poisson models", {
  df = data.frame(
    y = c(1, 2, 2, 3, 4, 5),
    x = c(0, 1, 2, 3, 4, 5)
  )

  model = stats::glm(
    y ~ x,
    data = df,
    family = stats::poisson(link = "log")
  )

  out = buildModelExplanationAudit(model)

  expect_identical(out$overview$modelFamily, "poisson")
  expect_identical(out$overview$link, "log")
  expect_identical(out$interpretationScale$fittedValueScale, "expected count")
  expect_identical(out$interpretationScale$effectScale, "expected-count multipliers")
  expect_match(
    out$interpretationScale$backTransformation,
    "expected-count scale",
    fixed = TRUE
  )
  expect_true(is.character(out$confidenceIntervals$displayedScales))
})
