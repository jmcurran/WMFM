test_that("buildEquationCases creates one overall case for intercept-only models", {
  df = data.frame(y = c(1, 2, 3, 4, 5))

  model = lm(y ~ 1, data = df)
  spec = buildEquationSpec(model)
  out = buildEquationCases(spec)

  expect_s3_class(out, "wmfmEquationCases")
  expect_length(out, 1)
  expect_identical(out[[1]]$caseId, "overall")
  expect_identical(out[[1]]$label, "Overall")
  expect_identical(out[[1]]$factorValues, list())
  expect_identical(out[[1]]$retainedNumeric, character(0))
})


test_that("buildEquationCases creates one general case for numeric-only models", {
  df = data.frame(
    y = c(2, 4, 5, 7, 9),
    x = c(0, 1, 2, 3, 4)
  )

  model = lm(y ~ x, data = df)
  spec = buildEquationSpec(model)
  out = buildEquationCases(spec)

  expect_length(out, 1)
  expect_identical(out[[1]]$caseId, "general")
  expect_identical(out[[1]]$label, "General")
  expect_identical(out[[1]]$factorValues, list())
  expect_identical(out[[1]]$retainedNumeric, "x")
})


test_that("buildEquationCases creates one case per level for one factor predictor", {
  df = data.frame(
    y = c(1, 2, 3, 4, 5, 6),
    group = factor(c("A", "A", "A", "B", "B", "B"))
  )

  model = lm(y ~ group, data = df)
  spec = buildEquationSpec(model)
  out = buildEquationCases(spec)

  expect_length(out, 2)
  expect_identical(vapply(out, `[[`, character(1), "label"), c(
    "group = A",
    "group = B"
  ))
  expect_identical(out[[1]]$factorValues, list(group = "A"))
  expect_identical(out[[2]]$factorValues, list(group = "B"))
  expect_identical(out[[1]]$retainedNumeric, character(0))
})


test_that("buildEquationCases retains numeric predictors within factor cases", {
  df = data.frame(
    y = c(1, 2, 4, 5, 2, 3, 5, 6),
    x = c(0, 1, 0, 1, 2, 3, 2, 3),
    group = factor(c("A", "A", "B", "B", "A", "A", "B", "B"))
  )

  model = lm(y ~ x * group, data = df)
  spec = buildEquationSpec(model)
  out = buildEquationCases(spec)

  expect_length(out, 2)
  expect_identical(vapply(out, `[[`, character(1), "label"), c(
    "group = A",
    "group = B"
  ))
  expect_identical(out[[1]]$retainedNumeric, "x")
  expect_identical(out[[2]]$retainedNumeric, "x")
})


test_that("buildEquationCases enumerates combinations for multiple factor predictors", {
  df = data.frame(
    y = c(1, 2, 3, 4, 5, 6, 7, 8),
    group = factor(c("A", "A", "A", "A", "B", "B", "B", "B")),
    sex = factor(c("F", "F", "M", "M", "F", "F", "M", "M"))
  )

  model = lm(y ~ group * sex, data = df)
  spec = buildEquationSpec(model)
  out = buildEquationCases(spec)

  expect_length(out, 4)
  expect_identical(vapply(out, `[[`, character(1), "label"), c(
    "group = A, sex = F",
    "group = B, sex = F",
    "group = A, sex = M",
    "group = B, sex = M"
  ))
  expect_identical(out[[4]]$factorValues, list(group = "B", sex = "M"))
})


test_that("buildEquationCases preserves family metadata for later rendering stages", {
  df = data.frame(
    pass = factor(c("Fail", "Fail", "Pass", "Pass", "Fail", "Pass")),
    x = c(0, 1, 0, 1, 2, 2)
  )

  model = glm(pass ~ x, data = df, family = binomial(link = "logit"))
  spec = buildEquationSpec(model)
  out = buildEquationCases(spec)

  expect_length(out, 1)
  expect_identical(spec$family, "binomial")
  expect_identical(spec$link, "logit")
  expect_identical(out[[1]]$retainedNumeric, "x")
})
