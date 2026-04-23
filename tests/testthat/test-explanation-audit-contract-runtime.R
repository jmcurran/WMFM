test_that("buildAppExplanationAudit returns a validated audit contract", {
  df = data.frame(
    y = c(1, 2, 3, 4, 5),
    x = c(-2, -1, 0, 1, 2)
  )
  model = stats::lm(y ~ x, data = df)

  audit = buildAppExplanationAudit(model = model)

  expect_s3_class(audit, "wmfmExplanationAudit")
  expect_invisible(validateWmfmExplanationAudit(audit))
})
