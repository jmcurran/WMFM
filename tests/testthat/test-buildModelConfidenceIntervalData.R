testthat::test_that("buildModelConfidenceIntervalData builds derived rows for simple lm models", {

  d = data.frame(
    Exam = c(10, 12, 11, 20, 22, 21),
    Attend = factor(c("No", "No", "No", "Yes", "Yes", "Yes")),
    Test = c(1, 2, 3, 1, 2, 3)
  )

  fit = lm(Exam ~ Attend + Test, data = d)

  out = buildModelConfidenceIntervalData(fit)

  testthat::expect_identical(out$mode, "derived")
  testthat::expect_true(is.data.frame(out$table))
  testthat::expect_true(any(grepl("Expected Exam when Attend = No", out$table$quantity, fixed = TRUE)))
  testthat::expect_true(any(grepl("Expected Exam when Attend = Yes", out$table$quantity, fixed = TRUE)))
  testthat::expect_true(any(grepl("Change in Exam for a 1-unit increase in Test", out$table$quantity, fixed = TRUE)))
  testthat::expect_true(any(grepl(
    "Cov",
    vapply(out$details, `[[`, character(1), "varianceFormula"),
    fixed = TRUE
  )))
})

testthat::test_that("buildModelConfidenceIntervalData falls back to coefficient mode for interactions", {

  d = data.frame(
    y = c(1, 2, 3, 4, 5, 6),
    g = factor(c("a", "a", "b", "b", "a", "b")),
    x = c(0, 1, 0, 1, 2, 2)
  )

  fit = lm(y ~ g * x, data = d)

  out = buildModelConfidenceIntervalData(fit)

  testthat::expect_identical(out$mode, "coefficient")
  testthat::expect_true(any(grepl("interaction terms", out$note, fixed = TRUE)))
})
