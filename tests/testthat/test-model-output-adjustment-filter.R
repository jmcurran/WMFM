test_that("filterSummaryCoefficientRows removes adjustment-related rows", {
  coefs = matrix(
    c(1, 2, 3, 4, 5, 6),
    ncol = 2,
    dimnames = list(
      c("(Intercept)", "gendermale", "picturelandscape"),
      c("Estimate", "Std. Error")
    )
  )

  out = filterSummaryCoefficientRows(
    coefficientsMatrix = coefs,
    adjustmentVariables = "picture",
    showAdjustmentCoefficients = FALSE
  )

  expect_equal(rownames(out), c("(Intercept)", "gendermale"))
})

test_that("filterSummaryCoefficientRows removes interactions involving adjustment variables", {
  coefs = matrix(
    c(1, 2, 3, 4, 5, 6, 7, 8),
    ncol = 2,
    dimnames = list(
      c("(Intercept)", "gendermale", "picturelandscape", "gendermale:picturelandscape"),
      c("Estimate", "Std. Error")
    )
  )

  out = filterSummaryCoefficientRows(
    coefficientsMatrix = coefs,
    adjustmentVariables = "picture",
    showAdjustmentCoefficients = FALSE
  )

  expect_equal(rownames(out), c("(Intercept)", "gendermale"))
})

test_that("filterAnovaTermRows keeps full table when showAdjustmentTerms is TRUE", {
  anovaTable = data.frame(Df = c(1, 1), check.names = FALSE)
  rownames(anovaTable) = c("gender", "gender:picture")

  out = filterAnovaTermRows(
    anovaTable = anovaTable,
    adjustmentVariables = "picture",
    showAdjustmentTerms = TRUE
  )

  expect_equal(rownames(out), c("gender", "gender:picture"))
})
