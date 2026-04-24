testthat::test_that("buildModelConfidenceIntervalData builds derived rows for simple lm models", {

  d = data.frame(
    Exam = c(10, 12, 11, 20, 22, 21),
    Attend = factor(c("No", "No", "No", "Yes", "Yes", "Yes")),
    Test = c(1, 2, 3)
  )

  d = d[rep(seq_len(nrow(d)), length.out = 6), ]
  d$Exam = c(10, 12, 11, 20, 22, 21)
  d$Attend = factor(c("No", "No", "No", "Yes", "Yes", "Yes"))
  d$Test = c(1, 2, 3, 1, 2, 3)

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

testthat::test_that("buildModelConfidenceIntervalData falls back to coefficient mode for lm interactions", {

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

testthat::test_that("buildModelConfidenceIntervalData gives derived odds-scale rows for logistic interactions", {

  d = data.frame(
    Pass = factor(
      c(
        "No", "No", "Yes", "No", "Yes", "Yes",
        "No", "Yes", "No", "Yes", "No", "Yes"
      ),
      levels = c("No", "Yes")
    ),
    Attend = factor(
      c(
        "No", "No", "No", "Yes", "Yes", "Yes",
        "No", "No", "Yes", "Yes", "No", "Yes"
      ),
      levels = c("No", "Yes")
    ),
    Score = c(0, 1, 2, 0, 1, 2, 3, 4, 3, 4, 5, 5)
  )

  fit = glm(Pass ~ Attend * Score, data = d, family = binomial())

  out = buildModelConfidenceIntervalData(fit, numericReference = "zero")

  testthat::expect_identical(out$mode, "derived")
  testthat::expect_true(any(grepl("Pr(Pass = Yes) when Attend = No", out$table$quantity, fixed = TRUE)))
  testthat::expect_true(any(grepl("Pr(Pass = No) when Attend = No", out$table$quantity, fixed = TRUE)))
  testthat::expect_true(any(grepl("Odds(Pass = Yes) when Attend = No", out$table$quantity, fixed = TRUE)))
  testthat::expect_true(any(grepl(
    "Odds(Pass = Yes) multiplier for a 1-unit increase in Score when Attend = Yes",
    out$table$quantity,
    fixed = TRUE
  )))

  detailIndex = which(vapply(
    out$details,
    function(x) {
      identical(x$quantity, "Odds(Pass = Yes) multiplier for a 1-unit increase in Score when Attend = Yes")
    },
    logical(1)
  ))

  testthat::expect_length(detailIndex, 1)
  testthat::expect_match(out$details[[detailIndex]]$varianceFormula, "Cov", fixed = TRUE)
})


testthat::test_that("buildModelConfidenceIntervalData reports mean anchoring when numericReference is mean", {

  d = data.frame(
    Freq = c(32, 27, 10, 9, 6, 6, 3, 2, 2, 13, 6, 2, 1, 0, 0, 1, 0, 0),
    Locn = factor(
      c(rep("SC", 9), rep("WA", 9)),
      levels = c("SC", "WA")
    ),
    Magnitude = c(5.25, 5.50, 5.75, 6.00, 6.25, 6.50, 6.75, 7.00, 7.25,
      5.25, 5.50, 5.75, 6.00, 6.25, 6.50, 6.75, 7.00, 7.25)
  )

  fit = glm(Freq ~ Locn * Magnitude, data = d, family = poisson())

  out = buildModelConfidenceIntervalData(fit, numericReference = "mean")

  testthat::expect_identical(out$mode, "derived")
  testthat::expect_match(out$note, "Numeric predictors are fixed at their means", fixed = TRUE)
  testthat::expect_true(any(grepl("Magnitude = 6.25", vapply(out$details, `[[`, character(1), "settings"), fixed = TRUE)))
})
