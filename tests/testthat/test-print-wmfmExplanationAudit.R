test_that("print.wmfmExplanationAudit shows the main audit sections", {
  df = data.frame(
    y = c(10, 12, 14, 16, 18, 20),
    x = c(-2, -1, 0, 1, 2, 3),
    grp = factor(c("A", "A", "B", "B", "A", "B"))
  )

  model = stats::lm(y ~ x + grp, data = df)
  attr(model, "wmfm_research_question") = "How does y change with x and grp?"
  attr(model, "wmfm_dataset_doc") = "A small deterministic example dataset"

  audit = buildModelExplanationAudit(model)

  out = testthat::capture_output(print(audit))

  expect_match(out, "WMFM explanation audit", fixed = TRUE)
  expect_match(out, "Overview", fixed = TRUE)
  expect_match(out, "Interpretation scale", fixed = TRUE)
  expect_match(out, "Numeric anchors", fixed = TRUE)
  expect_match(out, "Reference levels", fixed = TRUE)
  expect_match(out, "Confidence intervals", fixed = TRUE)
  expect_match(out, "Baseline evidence rows", fixed = TRUE)
  expect_match(out, "Effect evidence rows", fixed = TRUE)
  expect_match(out, "Coefficient rows", fixed = TRUE)
  expect_match(out, "Response: y", fixed = TRUE)
  expect_match(out, "Model family: gaussian", fixed = TRUE)
})

test_that("print.wmfmExplanationAudit handles audits without numeric anchor rows", {
  df = data.frame(
    y = c(5, 6, 7, 8, 9, 10),
    grp = factor(c("A", "A", "B", "B", "C", "C"))
  )

  model = stats::lm(y ~ grp, data = df)
  audit = buildModelExplanationAudit(model)

  out = testthat::capture_output(print(audit))

  expect_match(out, "Numeric anchors", fixed = TRUE)
  expect_match(out, "Reference rule: zero", fixed = TRUE)
  expect_match(out, "(none)", fixed = TRUE)
  expect_match(out, "Reference levels", fixed = TRUE)
})
