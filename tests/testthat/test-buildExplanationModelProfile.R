makeProfileLmNumericData = function() {
  set.seed(1)
  x = seq_len(12)
  y = 2 + 0.5 * x + stats::rnorm(length(x), sd = 0.2)
  data.frame(y = y, x = x)
}

makeProfileLmFactorData = function() {
  set.seed(1)
  group = factor(rep(c("A", "B"), each = 6))
  y = 2 + ifelse(group == "B", 1, 0) + stats::rnorm(length(group), sd = 0.2)
  data.frame(y = y, group = group)
}

makeProfileLmInteractionData = function() {
  set.seed(1)
  x = rep(seq_len(6), times = 2)
  group = factor(rep(c("A", "B"), each = 6))
  y = 1 + 0.4 * x + ifelse(group == "B", 0.8, 0) +
    ifelse(group == "B", 0.25 * x, 0) + stats::rnorm(length(x), sd = 0.2)
  data.frame(y = y, x = x, group = group)
}

testthat::test_that("buildExplanationModelProfile detects intercept-only LM models", {
  set.seed(1)
  df = data.frame(y = 2 + stats::rnorm(12, sd = 0.2))
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
  df = makeProfileLmNumericData()
  model = stats::lm(y ~ x, data = df)

  out = buildExplanationModelProfile(model = model, data = df, modelType = "lm")

  testthat::expect_identical(out$modelFamily, "lm")
  testthat::expect_identical(out$modelStructure, "numericMainEffect")
  testthat::expect_identical(out$predictorTypes$numeric, "x")
  testthat::expect_identical(out$predictorTypes$factor, character(0))
  testthat::expect_true(out$requiresAnchor)
})

testthat::test_that("buildExplanationModelProfile detects factor LM models", {
  df = makeProfileLmFactorData()
  model = stats::lm(y ~ group, data = df)

  out = buildExplanationModelProfile(model = model, data = df, modelType = "lm")

  testthat::expect_identical(out$modelStructure, "factorMainEffect")
  testthat::expect_identical(out$predictorTypes$factor, "group")
  testthat::expect_identical(out$comparisonScope, "minimal")
  testthat::expect_false(out$requiresAnchor)
})

testthat::test_that("buildExplanationModelProfile detects interactions", {
  df = makeProfileLmInteractionData()
  model = stats::lm(y ~ x * group, data = df)

  out = buildExplanationModelProfile(model = model, data = df, modelType = "lm")

  testthat::expect_identical(out$modelStructure, "interaction")
  testthat::expect_true(out$hasInteractions)
  testthat::expect_true("numericByFactor" %in% out$interactionTypes)
  testthat::expect_identical(out$comparisonScope, "targeted")
})

testthat::test_that("buildExplanationModelProfile detects logistic scale metadata", {
  set.seed(1)
  df = data.frame(
    y = c(0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0),
    x = stats::rnorm(12)
  )
  model = stats::glm(y ~ x, data = df, family = stats::binomial(link = "logit"))

  out = buildExplanationModelProfile(model = model, data = df, modelType = "logistic")

  testthat::expect_identical(out$modelFamily, "logistic")
  testthat::expect_identical(out$modelScale, "logOdds")
  testthat::expect_identical(out$interpretationScale, "probability")
})

testthat::test_that("buildExplanationModelProfile detects Poisson scale metadata", {
  set.seed(1)
  x = seq_len(12)
  df = data.frame(
    y = stats::rpois(length(x), lambda = exp(0.4 + 0.08 * x)),
    x = x
  )
  model = stats::glm(y ~ x, data = df, family = stats::poisson(link = "log"))

  out = buildExplanationModelProfile(model = model, data = df, modelType = "poisson")

  testthat::expect_identical(out$modelFamily, "poisson")
  testthat::expect_identical(out$modelScale, "logExpectedCount")
  testthat::expect_identical(out$interpretationScale, "expectedCount")
})

testthat::test_that("buildExplanationModelProfile records transformed response metadata", {
  set.seed(1)
  x = seq_len(12)
  df = data.frame(
    y = exp(1 + 0.1 * x + stats::rnorm(length(x), sd = 0.05)),
    x = x
  )
  model = stats::lm(log(y) ~ x, data = df)

  out = buildExplanationModelProfile(model = model, data = df, modelType = "lm")

  testthat::expect_identical(out$responseVariable, "y")
  testthat::expect_identical(out$transformationType, "log")
  testthat::expect_identical(out$modelScale, "log(y)")
  testthat::expect_identical(out$interpretationScale, "originalResponse")
})

testthat::test_that("newWmfmModel stores explanation model profile metadata", {
  df = makeProfileLmNumericData()
  model = stats::lm(y ~ x, data = df)
  profile = buildExplanationModelProfile(model = model, data = df, modelType = "lm")

  out = newWmfmModel(
    model = model,
    formula = y ~ x,
    modelType = "lm",
    data = df,
    modelProfile = profile
  )

  testthat::expect_type(out$modelProfile, "list")
  testthat::expect_identical(out$modelProfile$modelFamily, "lm")
  testthat::expect_identical(out$modelProfile$modelStructure, "numericMainEffect")
})
