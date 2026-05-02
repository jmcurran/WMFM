test_that("buildGlmTeachingNotation returns outcome-aware binomial labels", {
  df = data.frame(
    pass = factor(c("Fail", "Fail", "Pass", "Pass", "Pass", "Fail")),
    x = c(0, 1, 0, 1, 2, 2)
  )

  model = glm(pass ~ x, data = df, family = binomial(link = "logit"))

  out = buildGlmTeachingNotation(model)

  expect_identical(out$probabilitySuccess, "Pr(pass = \"Pass\")")
  expect_identical(out$probabilityFailure, "Pr(pass = \"Fail\")")
  expect_identical(out$oddsSuccess, "Odds(pass = \"Pass\" vs pass = \"Fail\")")
  expect_identical(out$oddsFailure, "Odds(pass = \"Fail\" vs pass = \"Pass\")")
  expect_identical(out$logitSuccess, "logit(Pr(pass = \"Pass\"))")
})


test_that("makeMeanEquation shows odds and probabilities for binomial GLMs", {
  df = data.frame(
    pass = factor(c("Fail", "Fail", "Pass", "Pass", "Pass", "Fail")),
    x = c(0, 1, 0, 1, 2, 2)
  )

  model = glm(pass ~ x, data = df, family = binomial(link = "logit"))
  oneRow = data.frame(x = 1)

  out = makeMeanEquation(model, oneRowDf = oneRow, label = "Case")

  expect_match(out, 'logit\\(Pr\\(pass = "Pass"\\)\\)')
  expect_match(out, 'Odds\\(pass = "Pass" vs pass = "Fail"\\)')
  expect_match(out, 'Pr\\(pass = "Pass"\\)')
  expect_match(out, 'Odds\\(pass = "Fail" vs pass = "Pass"\\)')
  expect_match(out, 'Pr\\(pass = "Fail"\\)')
})


test_that("buildModelConfidenceIntervalData uses E(Y) notation for Poisson GLMs", {
  df = data.frame(
    y = c(1, 2, 2, 3, 4, 5, 6, 7),
    group = factor(rep(c("A", "B"), each = 4)),
    x = c(0, 1, 2, 3, 0, 1, 2, 3)
  )

  model = glm(y ~ group + x, data = df, family = poisson(link = "log"))

  out = buildModelConfidenceIntervalData(model, numericReference = "zero")

  expect_true(any(grepl("^E\\[y\\] when group = ", out$table$quantity)))
  expect_true(any(grepl("^E\\[y\\] multiplier for a 1-unit increase in x$", out$table$quantity)))
})
