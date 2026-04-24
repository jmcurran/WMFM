testthat::test_that("buildExplanationModelProfile detects intercept-only LM models", {
  df = data.frame(y = c(1, 2, 3, 4))
  model = stats::lm(y ~ 1, data = df)

  out = buildExplanationModelProfile(model = model, data = df, modelType = "lm")

  testthat::expect_identical(out$modelFamily, "lm")
  testthat::expect_identical(out$modelStructure, "interceptOnly")
  testthat::expect_identical(out$modelScale, "response")
  testthat::expect_identical(out$interpretationScale, "response")
  testthat::expect_identical(out$comparisonScope, "none")
  testthat::expect_false(out$requiresAnchor)
})

testthat::test_that("buildExplanationModelProfile detects numeric LM models", {
  df = data.frame(
    y = c(1, 2, 3, 4),
    x = c(2, 4, 6, 8)
  )
  model = stats::lm(y ~ x, data = df)

  out = buildExplanationModelProfile(model = model, data = df, modelType = "lm")

  testthat::expect_identical(out$modelFamily, "lm")
  testthat::expect_identical(out$modelStructure, "numericMainEffect")
  testthat::expect_identical(out$predictorTypes$numeric, "x")
  testthat::expect_identical(out$predictorTypes$factor, character(0))
  testthat::expect_true(out$requiresAnchor)
})

testthat::test_that("buildExplanationModelProfile detects factor LM models", {
  df = data.frame(
    y = c(1, 2, 3, 4),
    group = factor(c("A", "A", "B", "B"))
  )
  model = stats::lm(y ~ group, data = df)

  out = buildExplanationModelProfile(model = model, data = df, modelType = "lm")

  testthat::expect_identical(out$modelStructure, "factorMainEffect")
  testthat::expect_identical(out$predictorTypes$factor, "group")
  testthat::expect_identical(out$comparisonScope, "minimal")
  testthat::expect_false(out$requiresAnchor)
})

testthat::test_that("buildExplanationModelProfile detects interactions", {
  df = data.frame(
    y = c(1, 2, 3, 4, 5, 6),
    x = c(1, 2, 3, 1, 2, 3),
    group = factor(c("A", "A", "A", "B", "B", "B"))
  )
  model = stats::lm(y ~ x * group, data = df)

  out = buildExplanationModelProfile(model = model, data = df, modelType = "lm")

  testthat::expect_identical(out$modelStructure, "interaction")
  testthat::expect_true(out$hasInteractions)
  testthat::expect_true("numericByFactor" %in% out$interactionTypes)
  testthat::expect_identical(out$comparisonScope, "targeted")
})

testthat::test_that("buildExplanationModelProfile detects logistic scale metadata", {
  df = data.frame(
    y = c(0, 0, 1, 1, 1, 0),
    x = c(1, 2, 3, 4, 5, 6)
  )
  model = stats::glm(y ~ x, data = df, family = stats::binomial(link = "logit"))

  out = buildExplanationModelProfile(model = model, data = df, modelType = "logistic")

  testthat::expect_identical(out$modelFamily, "logistic")
  testthat::expect_identical(out$modelScale, "logOdds")
  testthat::expect_identical(out$interpretationScale, "probability")
})

testthat::test_that("buildExplanationModelProfile detects Poisson scale metadata", {
  df = data.frame(
    y = c(1, 2, 3, 4, 5, 6),
    x = c(1, 2, 3, 4, 5, 6)
  )
  model = stats::glm(y ~ x, data = df, family = stats::poisson(link = "log"))

  out = buildExplanationModelProfile(model = model, data = df, modelType = "poisson")

  testthat::expect_identical(out$modelFamily, "poisson")
  testthat::expect_identical(out$modelScale, "logExpectedCount")
  testthat::expect_identical(out$interpretationScale, "expectedCount")
})

testthat::test_that("buildExplanationModelProfile records transformed response metadata", {
  df = data.frame(
    y = c(1, 2, 3, 4, 5, 6),
    x = c(1, 2, 3, 4, 5, 6)
  )
  model = stats::lm(log(y) ~ x, data = df)

  out = buildExplanationModelProfile(model = model, data = df, modelType = "lm")

  testthat::expect_identical(out$responseVariable, "y")
  testthat::expect_identical(out$transformationType, "log")
  testthat::expect_identical(out$modelScale, "log(y)")
  testthat::expect_identical(out$interpretationScale, "originalResponse")
})

testthat::test_that("runModel attaches explanation model profile metadata", {
  df = data.frame(
    y = c(1, 2, 3, 4),
    x = c(1, 2, 3, 4)
  )

  out = suppressWarnings(
    runModel(
      data = df,
      formula = y ~ x,
      modelType = "lm",
      printOutput = FALSE,
      equationMethod = "deterministic"
    )
  )

  testthat::expect_type(out$modelProfile, "list")
  testthat::expect_identical(out$modelProfile$modelFamily, "lm")
  testthat::expect_identical(out$modelProfile$modelStructure, "numericMainEffect")
})
