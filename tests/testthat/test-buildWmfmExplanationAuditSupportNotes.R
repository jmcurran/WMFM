test_that("buildWmfmExplanationAuditSupportNotes returns stable handoff notes", {
  df = data.frame(
    y = c(10, 12, 14, 16, 18, 20),
    x = c(-2, -1, 0, 1, 2, 3),
    grp = factor(c("A", "A", "B", "B", "A", "B"))
  )

  model = stats::lm(y ~ x + grp, data = df)
  audit = buildModelExplanationAudit(model)
  out = buildWmfmExplanationAuditSupportNotes(audit)

  expect_true(is.data.frame(out))
  expect_identical(
    names(out),
    c("section", "purpose", "downstreamUse")
  )
  expect_identical(
    out$section,
    c(
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
    )
  )

  expect_true(any(grepl("teaching-summary", out$downstreamUse, fixed = TRUE)))
  expect_true(any(grepl("tolerate zero rows", out$downstreamUse, fixed = TRUE)))
  expect_true(any(grepl("Keep internal", out$downstreamUse, fixed = TRUE)))
})


test_that("buildWmfmExplanationAuditSupportNotes validates audit input", {
  badAudit = list()
  class(badAudit) = "wmfmExplanationAudit"

  expect_error(
    buildWmfmExplanationAuditSupportNotes(badAudit),
    "missing required explanation-audit field"
  )
})
