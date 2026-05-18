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

test_that("isAdjustmentRelatedOutputRow matches factor-style summary labels", {
  expect_true(isAdjustmentRelatedOutputRow("picturelandscape", "picture"))
  expect_true(isAdjustmentRelatedOutputRow("gendermale:picturelandscape", "picture"))
  expect_true(isAdjustmentRelatedOutputRow("`picture`:gendermale", "`picture`"))
  expect_false(isAdjustmentRelatedOutputRow("gendermale", "picture"))
})


test_that("filterConfidenceIntervalRows removes adjustment-related quantities", {
  ciTable = data.frame(
    quantity = c(
      "Expected Exam when picture = portrait",
      "Difference in Exam comparing gender = male with gender = female",
      "Difference in Exam comparing picture = landscape with picture = portrait"
    ),
    estimate = c(10, 2, 1),
    stringsAsFactors = FALSE
  )

  out = filterConfidenceIntervalRows(ciTable = ciTable, adjustmentVariables = "picture")

  expect_equal(
    out$quantity,
    "Difference in Exam comparing gender = male with gender = female"
  )
})
