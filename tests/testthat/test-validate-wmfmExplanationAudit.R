test_that("validateWmfmExplanationAudit accepts a complete audit", {
  df = data.frame(
    y = c(1, 2, 3, 4, 5),
    x = c(-2, -1, 0, 1, 2)
  )

  audit = buildModelExplanationAudit(stats::lm(y ~ x, data = df))

  expect_invisible(validateWmfmExplanationAudit(audit))
})

test_that("validateWmfmExplanationAudit accepts NULL when allowNull is TRUE", {
  expect_invisible(validateWmfmExplanationAudit(NULL, allowNull = TRUE))
})

test_that("validateWmfmExplanationAudit rejects objects without the audit class", {
  badAudit = list()

  expect_error(
    validateWmfmExplanationAudit(badAudit),
    "must inherit from `wmfmExplanationAudit`"
  )
})

test_that("validateWmfmExplanationAudit rejects audits with missing required fields", {
  df = data.frame(
    y = c(1, 2, 3, 4, 5),
    x = c(-2, -1, 0, 1, 2)
  )

  audit = buildModelExplanationAudit(stats::lm(y ~ x, data = df))
  audit$promptRules = NULL

  expect_error(
    validateWmfmExplanationAudit(audit),
    "missing required explanation-audit fields: promptRules"
  )
})

test_that("newWmfmModel rejects invalid explanationAudit values", {
  df = data.frame(
    y = c(1, 2, 3, 4, 5),
    x = c(-2, -1, 0, 1, 2)
  )
  model = stats::lm(y ~ x, data = df)

  expect_error(
    newWmfmModel(
      model = model,
      formula = y ~ x,
      modelType = "lm",
      data = df,
      explanationAudit = list(bad = TRUE)
    ),
    "must inherit from `wmfmExplanationAudit`"
  )
})
