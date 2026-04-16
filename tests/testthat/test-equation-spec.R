test_that("buildEquationSpec captures intercept-only lm structure", {
  df = data.frame(y = c(1, 2, 3, 4, 5))

  model = lm(y ~ 1, data = df)
  out = buildEquationSpec(model)

  expect_s3_class(out, "wmfmEquationSpec")
  expect_identical(out$family, "gaussian")
  expect_identical(out$link, "identity")
  expect_identical(out$responseName, "y")
  expect_identical(out$predictorNames, character(0))
  expect_length(out$predictors, 0)
  expect_identical(out$coefficients$term, "(Intercept)")
  expect_length(out$interactions, 0)
})


test_that("buildEquationSpec captures numeric predictor metadata", {
  df = data.frame(
    y = c(2, 4, 5, 7, 9),
    x = c(0, 1, 2, 3, 4)
  )

  model = lm(y ~ x, data = df)
  out = buildEquationSpec(model)

  expect_identical(out$predictorNames, "x")
  expect_identical(out$predictors[["x"]]$name, "x")
  expect_identical(out$predictors[["x"]]$type, "numeric")
  expect_null(out$predictors[["x"]]$levels)
  expect_null(out$predictors[["x"]]$reference)
  expect_true(all(c("(Intercept)", "x") %in% out$coefficients$term))
})


test_that("buildEquationSpec captures factor predictor metadata and reference level", {
  df = data.frame(
    y = c(1, 2, 3, 4, 5, 6),
    group = factor(c("A", "A", "A", "B", "B", "B"))
  )

  model = lm(y ~ group, data = df)
  out = buildEquationSpec(model)

  expect_identical(out$predictors[["group"]]$type, "factor")
  expect_identical(out$predictors[["group"]]$levels, c("A", "B"))
  expect_identical(out$predictors[["group"]]$reference, "A")
  expect_true(any(grepl("^group", out$coefficients$term)))
})


test_that("buildEquationSpec detects numeric-factor interactions", {
  df = data.frame(
    y = c(1, 2, 4, 5, 2, 3, 5, 6),
    x = c(0, 1, 0, 1, 2, 3, 2, 3),
    group = factor(c("A", "A", "B", "B", "A", "A", "B", "B"))
  )

  model = lm(y ~ x * group, data = df)
  out = buildEquationSpec(model)

  expect_length(out$interactions, 1)
  expect_identical(out$interactions[[1]]$term, "x:group")
  expect_identical(out$interactions[[1]]$vars, c("x", "group"))
  expect_identical(out$interactions[[1]]$type, "numeric:factor")
})


test_that("buildEquationSpec carries binomial notation metadata", {
  df = data.frame(
    pass = factor(c("Fail", "Fail", "Pass", "Pass", "Fail", "Pass")),
    x = c(0, 1, 0, 1, 2, 2)
  )

  model = glm(pass ~ x, data = df, family = binomial(link = "logit"))
  out = buildEquationSpec(model)

  expect_identical(out$family, "binomial")
  expect_identical(out$link, "logit")
  expect_identical(out$notation$probabilitySuccess, "Pr(Y = Pass)")
  expect_identical(out$notation$oddsSuccess, "Odds(Y = Pass)")
})


test_that("buildEquationSpec carries Poisson family metadata", {
  df = data.frame(
    y = c(1, 2, 2, 3, 4, 5),
    x = c(0, 1, 2, 3, 4, 5)
  )

  model = glm(y ~ x, data = df, family = poisson(link = "log"))
  out = buildEquationSpec(model)

  expect_identical(out$family, "poisson")
  expect_identical(out$link, "log")
  expect_identical(out$notation$mean, "E(Y)")
  expect_identical(out$notation$logMean, "log(E(Y))")
})
