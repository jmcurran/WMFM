test_that("buildModelExplanationAudit returns the required contract sections for a linear model", {
  df = data.frame(
    y = c(10, 12, 14, 16, 18, 20),
    x = c(-2, -1, 0, 1, 2, 3),
    grp = factor(c("A", "A", "B", "B", "A", "B"))
  )

  model = stats::lm(y ~ x + grp, data = df)
  attr(model, "wmfm_research_question") = "How does y change with x and grp?"
  attr(model, "wmfm_dataset_doc") = "A small deterministic example dataset"
  attr(model, "wmfm_dataset_name") = "contract-demo"
  attr(model, "wmfm_response_noun_phrase") = "outcome"

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
  expect_match(out$transparencyNote, "does not claim to reveal hidden chain-of-thought")

  expect_identical(out$overview$response, "y")
  expect_identical(out$overview$predictors, c("x", "grp"))
  expect_identical(out$overview$nObservations, 6L)
  expect_identical(out$overview$modelFamily, "gaussian")
  expect_identical(out$overview$link, "identity")
  expect_identical(out$overview$hasDatasetContext, TRUE)
  expect_identical(out$overview$hasResearchQuestion, TRUE)

  expect_identical(out$promptInputs$response, "y")
  expect_identical(out$promptInputs$responseNounPhrase, "outcome")
  expect_identical(out$promptInputs$predictors, c("x", "grp"))
  expect_identical(out$promptInputs$datasetContextUsed, TRUE)
  expect_identical(out$promptInputs$datasetName, "contract-demo")
  expect_identical(out$promptInputs$researchQuestionUsed, TRUE)
  expect_identical(out$promptInputs$researchQuestion, "How does y change with x and grp?")
  expect_identical(out$promptInputs$coefficientTableIncluded, TRUE)
  expect_identical(out$promptInputs$confidenceIntervalsIncluded, TRUE)
  expect_identical(out$promptInputs$precomputedBaselineValuesIncluded, TRUE)
  expect_identical(out$promptInputs$numericAnchorRuleIncluded, TRUE)
  expect_identical(out$promptInputs$nObservations, 6L)

  expect_true(is.character(out$promptRules))
  expect_true(length(out$promptRules) >= 5)
  expect_true(any(grepl("plain language", out$promptRules, fixed = TRUE)))
  expect_true(any(grepl("numeric anchor", out$promptRules, fixed = TRUE)))
  expect_true(any(grepl("research question", out$promptRules, fixed = TRUE)))

  expect_identical(out$interpretationScale$responseExpression, "y")
  expect_identical(out$interpretationScale$responseTransform, "none")
  expect_identical(out$interpretationScale$fittedValueScale, "response scale")
  expect_identical(out$interpretationScale$effectScale, "additive response-scale differences")
  expect_match(out$interpretationScale$backTransformation, "No back-transformation is required.", fixed = TRUE)

  expect_identical(out$numericAnchor$numericReference, "zero")
  expect_true(is.character(out$numericAnchor$note))
  expect_true(is.data.frame(out$numericAnchor$table))
  expect_identical(names(out$numericAnchor$table), c("predictor", "observedRange", "anchor", "reason"))
  expect_identical(out$numericAnchor$table$predictor, "x")
  expect_equal(out$numericAnchor$table$anchor, 0)
  expect_true(any(grepl("inside the observed range", out$numericAnchor$table$reason, fixed = TRUE)))

  expect_true(is.data.frame(out$referenceLevels))
  expect_identical(names(out$referenceLevels), c("predictor", "referenceLevel", "levels"))
  expect_identical(out$referenceLevels$predictor, "grp")
  expect_identical(out$referenceLevels$referenceLevel, "A")
  expect_identical(out$referenceLevels$levels, "A, B")

  expect_true(is.list(out$confidenceIntervals))
  expect_true(all(c("level", "mode", "note", "teachingNote", "displayedScales") %in% names(out$confidenceIntervals)))
  expect_identical(out$confidenceIntervals$level, 0.95)
  expect_true(is.character(out$confidenceIntervals$displayedScales))

  expect_true(is.data.frame(out$baselineEvidence))
  expect_true(nrow(out$baselineEvidence) >= 1)
  expect_true(all(c("ciSection", "quantity", "estimate", "lower", "upper", "scale", "displayScale") %in% names(out$baselineEvidence)))

  expect_true(is.data.frame(out$effectEvidence))
  expect_true(nrow(out$effectEvidence) >= 1)
  expect_true(all(c("ciSection", "quantity", "estimate", "lower", "upper", "scale", "displayScale") %in% names(out$effectEvidence)))

  expect_true(is.data.frame(out$coefficientTable))
  expect_true(nrow(out$coefficientTable) >= 2)
  expect_true("term" %in% names(out$coefficientTable))
  expect_true("(Intercept)" %in% out$coefficientTable$term)
  expect_true("x" %in% out$coefficientTable$term)

  expect_true(is.list(out$rawPromptIngredients))
  expect_true(all(c("languageContract", "numericAnchorPrompt", "anchoredBaselinePrompt") %in% names(out$rawPromptIngredients)))
  expect_true(all(vapply(out$rawPromptIngredients, is.character, logical(1))))
  expect_true(all(nzchar(vapply(out$rawPromptIngredients, function(x) {
    paste(x, collapse = "")
  }, character(1)))))
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
  expect_true(any(grepl("outside the observed range", out$numericAnchor$table$reason, fixed = TRUE)))
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
  expect_match(out$numericAnchor$note, "No numeric predictors were present", fixed = TRUE)
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
  expect_true(any(out$confidenceIntervals$displayedScales %in% c("probability", "odds", "odds multiplier")))
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
  expect_match(out$interpretationScale$backTransformation, "expected-count scale", fixed = TRUE)
  expect_true(any(out$confidenceIntervals$displayedScales %in% c("expected count", "count multiplier")))
})
