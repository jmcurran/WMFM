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

testthat::test_that("buildModelConfidenceIntervalData gives odds-scale rows for logistic interactions", {

  testthat::skip_if_not_installed("stats")

  set.seed(123)

  n = 300
  d = data.frame(
    Attend = factor(sample(c("No", "Yes"), size = n, replace = TRUE)),
    Test = stats::runif(n, min = 0, max = 10)
  )

  eta = -4.71 - 5.13 * (d$Attend == "Yes") + 0.42 * d$Test + 0.76 * (d$Attend == "Yes") * d$Test
  p = stats::plogis(eta)

  d$Result = factor(
    ifelse(stats::rbinom(n, size = 1, prob = p) == 1, "Pass", "Fail"),
    levels = c("Fail", "Pass")
  )

  fit = stats::glm(Result ~ Attend * Test, data = d, family = stats::binomial(link = "logit"))

  out = buildModelConfidenceIntervalData(fit, numericReference = "zero")

  testthat::expect_identical(out$mode, "derived")
  testthat::expect_true(any(grepl("Pr(Pass) when Attend = No", out$table$quantity, fixed = TRUE)))
  testthat::expect_true(any(grepl("Odds(Pass) when Attend = Yes", out$table$quantity, fixed = TRUE)))
  testthat::expect_true(any(grepl(
    "Odds(Pass) multiplier for a 1-unit increase in Test when Attend = Yes",
    out$table$quantity,
    fixed = TRUE
  )))

  targetLabel = "Odds(Pass) multiplier for a 1-unit increase in Test when Attend = Yes"
  detailIndex = which(vapply(out$details, function(x) identical(x$label, targetLabel), logical(1)))

  testthat::expect_length(detailIndex, 1)
  testthat::expect_match(out$details[[detailIndex]]$builtFrom, "AttendYes:Test")
  testthat::expect_match(out$details[[detailIndex]]$varianceFormula, "Cov", fixed = TRUE)
})
