test_that("renderEquationCases renders intercept-only Gaussian equations", {
  df = data.frame(y = c(3, 3, 3, 3))

  model = lm(y ~ 1, data = df)
  spec = buildEquationSpec(model)
  cases = buildEquationCases(spec)
  out = renderEquationCases(spec, cases)

  expect_s3_class(out, "wmfmEquationRender")
  expect_length(out, 1)
  expect_identical(out[[1]]$linearPredictor, "y = 3.00")
  expect_null(out[[1]]$oddsScale)
  expect_null(out[[1]]$responseScale)
})


test_that("renderEquationCases renders numeric-only Gaussian equations", {
  df = data.frame(
    x = 0:4,
    y = 1 + 2 * (0:4)
  )

  model = lm(y ~ x, data = df)
  spec = buildEquationSpec(model)
  cases = buildEquationCases(spec)
  out = renderEquationCases(spec, cases)

  expect_identical(out[[1]]$linearPredictor, "y = 1.00 + 2.00 * x")
  expect_identical(out[[1]]$simplifiedRhs, "1.00 + 2.00 * x")
})


test_that("renderEquationCases renders factor-only Gaussian equations with combined intercepts", {
  df = data.frame(
    group = factor(c("A", "A", "B", "B")),
    y = c(1, 1, 3, 3)
  )

  model = lm(y ~ group, data = df)
  spec = buildEquationSpec(model)
  cases = buildEquationCases(spec)
  out = renderEquationCases(spec, cases)

  expect_identical(out[[1]]$linearPredictor, "y = 1.00 (when group = A)")
  expect_identical(
    out[[2]]$linearPredictor,
    "y = (1.00 + 2.00) = 3.00 (when group = B)"
  )
})


test_that("renderEquationCases renders numeric-factor Gaussian interactions", {
  df = data.frame(
    x = c(0, 1, 2, 0, 1, 2),
    group = factor(c("A", "A", "A", "B", "B", "B"))
  )
  df$y = ifelse(df$group == "A", 1 + 2 * df$x, 4 + 6 * df$x)

  model = lm(y ~ x * group, data = df)
  spec = buildEquationSpec(model)
  cases = buildEquationCases(spec)
  out = renderEquationCases(spec, cases)

  expect_identical(
    out[[1]]$linearPredictor,
    "y = 1.00 + 2.00 * x (when group = A)"
  )
  expect_identical(
    out[[2]]$linearPredictor,
    "y = (1.00 + 3.00) + (2.00 + 4.00) * x = 4.00 + 6.00 * x (when group = B)"
  )
})


test_that("renderEquationCases renders logistic equations on logit, odds, and probability scales", {
  df = data.frame(
    pass = factor(c("Fail", "Pass", "Fail", "Pass", "Fail", "Pass", "Fail", "Pass")),
    x = c(-2, -1, 0, 0, 1, 1, 2, 3)
  )

  model = glm(pass ~ x, data = df, family = binomial(link = "logit"))
  spec = buildEquationSpec(model)
  cases = buildEquationCases(spec)
  out = renderEquationCases(spec, cases)

  expect_match(out[[1]]$linearPredictor, "^logit\\(Pr\\(Y = Pass\\)\\) = ")
  expect_match(out[[1]]$oddsScale, "^Odds\\(Y = Pass\\) = exp\\(")
  expect_match(out[[1]]$responseScale, "^Pr\\(Y = Pass\\) = exp\\(")
  expect_true(grepl(out[[1]]$simplifiedRhs, out[[1]]$oddsScale, fixed = TRUE))
  expect_true(grepl(out[[1]]$simplifiedRhs, out[[1]]$responseScale, fixed = TRUE))
})


test_that("renderEquationCases renders Poisson equations on log and mean scales", {
  df = data.frame(
    x = 0:5,
    y = c(1, 1, 2, 3, 5, 8)
  )

  model = glm(y ~ x, data = df, family = poisson(link = "log"))
  spec = buildEquationSpec(model)
  cases = buildEquationCases(spec)
  out = renderEquationCases(spec, cases)

  expect_match(out[[1]]$linearPredictor, "^log\\(E\\(Y\\)\\) = ")
  expect_match(out[[1]]$responseScale, "^E\\(Y\\) = exp\\(")
  expect_true(grepl(out[[1]]$simplifiedRhs, out[[1]]$responseScale, fixed = TRUE))
  expect_null(out[[1]]$oddsScale)
})
