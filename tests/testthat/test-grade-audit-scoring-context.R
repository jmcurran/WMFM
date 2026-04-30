test_that("grade run records carry audit-derived scoring context", {
  df = data.frame(
    y = c(0, 0, 1, 0, 1, 1, 0, 1),
    x = c(0, 1, 1, 2, 2, 3, 3, 4)
  )

  model = stats::glm(y ~ x, data = df, family = stats::binomial())
  audit = buildModelExplanationAudit(model = model)

  wm = newWmfmModel(
    model = model,
    formula = y ~ x,
    modelType = "glm",
    data = df,
    equations = "logit Pr(y = 1) = beta0 + beta1 * x",
    explanation = "As x increases, the odds of y = 1 increase.",
    explanationAudit = audit
  )

  record = buildWmfmGradeRunRecord(
    x = wm,
    explanation = paste(
      "For each one-unit increase in x, y is about 2 points higher.",
      "This describes an association rather than a causal effect."
    )
  )

  expect_true(record$hasExplanationAudit)
  expect_equal(record$modelFamily, "binomial")
  expect_equal(record$linkFunction, "logit")
  expect_equal(record$auditEffectScale, "odds multipliers")
  expect_equal(record$expectedEffectScale, "probability_or_odds")
  expect_equal(record$scoringContext, "final_text_plus_explanation_audit")
})


test_that("audit expected effect scale constrains deterministic grading", {
  df = data.frame(
    y = c(0, 0, 1, 0, 1, 1, 0, 1),
    x = c(0, 1, 1, 2, 2, 3, 3, 4)
  )

  model = stats::glm(y ~ x, data = df, family = stats::binomial())
  audit = buildModelExplanationAudit(model = model)

  wm = newWmfmModel(
    model = model,
    formula = y ~ x,
    modelType = "glm",
    data = df,
    equations = "logit Pr(y = 1) = beta0 + beta1 * x",
    explanation = "As x increases, the odds of y = 1 increase.",
    explanationAudit = audit
  )

  record = buildWmfmGradeRunRecord(
    x = wm,
    explanation = paste(
      "For each one-unit increase in x, y is about 2 points higher.",
      "This describes an association rather than a causal effect."
    )
  )

  scored = scoreWmfmRunRecordsCore(
    runsDf = as.data.frame(record, stringsAsFactors = FALSE),
    penaliseDuplicates = FALSE
  )

  expect_equal(scored$effectScaleClaim, "additive")
  expect_equal(scored$expectedEffectScale, "probability_or_odds")
  expect_equal(scored$effectScaleAppropriate, 0L)
})
