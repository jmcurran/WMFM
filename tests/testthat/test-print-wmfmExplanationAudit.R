test_that("print.wmfmExplanationAudit shows the main audit sections", {
  df = data.frame(
    y = c(10, 12, 14, 16, 18, 20),
    x = c(-2, -1, 0, 1, 2, 3),
    grp = factor(c("A", "A", "B", "B", "A", "B"))
  )

  model = stats::lm(y ~ x + grp, data = df)
  audit = buildModelExplanationAudit(model)

  out = paste(capture.output(print(audit)), collapse = "\n")

  expect_match(out, "WMFM explanation audit", fixed = TRUE)
  expect_match(out, "Overview", fixed = TRUE)
  expect_match(out, "Interpretation scale", fixed = TRUE)
  expect_match(out, "Numeric anchors", fixed = TRUE)
  expect_match(out, "Reference levels", fixed = TRUE)
  expect_match(out, "Confidence intervals", fixed = TRUE)
  expect_match(out, "Evidence tables", fixed = TRUE)

  expect_match(out, "- Response: y", fixed = TRUE)
  expect_match(out, "- Predictors: x, grp", fixed = TRUE)
  expect_match(out, "- Observations: 6", fixed = TRUE)
  expect_match(out, "- Family/link: gaussian / identity", fixed = TRUE)

  expect_match(out, "- Fitted-value scale: response scale", fixed = TRUE)
  expect_match(out, "- Effect scale: additive response-scale differences", fixed = TRUE)

  expect_match(out, "- Reference rule: zero", fixed = TRUE)
  expect_match(out, "predictor", fixed = TRUE)
  expect_match(out, "observedRange", fixed = TRUE)
  expect_match(out, "anchor", fixed = TRUE)
  expect_match(out, "reason", fixed = TRUE)

  expect_match(out, "- Baseline rows: ", fixed = TRUE)
  expect_match(out, "- Effect rows: ", fixed = TRUE)
  expect_match(out, "- Coefficient rows: ", fixed = TRUE)
})

test_that("print.wmfmExplanationAudit handles factor-only audits cleanly", {
  df = data.frame(
    y = c(5, 6, 7, 8, 9, 10),
    grp = factor(c("A", "A", "B", "B", "C", "C"))
  )

  model = stats::lm(y ~ grp, data = df)
  audit = buildModelExplanationAudit(model)

  out = paste(capture.output(print(audit)), collapse = "\n")

  expect_match(out, "Numeric anchors", fixed = TRUE)
  expect_match(out, "- Reference rule: zero", fixed = TRUE)
  expect_match(out, "\\(none\\)")
})

test_that("print.wmfmExplanationAudit returns invisibly", {
  df = data.frame(
    y = c(1, 2, 3, 4),
    x = c(0, 1, 2, 3)
  )

  model = stats::lm(y ~ x, data = df)
  audit = buildModelExplanationAudit(model)

  returned = print(audit)

  expect_identical(returned, invisible(audit))
})
