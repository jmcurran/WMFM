test_that("chat provider fit messages preserve fallback wording", {
  msg = buildChatProviderConnectionFailedMessage("connection refused")

  expect_match(msg, "Could not connect to the language model server", fixed = TRUE)
  expect_match(msg, "equations/explanation", fixed = TRUE)
  expect_match(msg, "connection refused", fixed = TRUE)

  expect_match(buildNoLanguageModelAvailableMessage(), "No language model is available", fixed = TRUE)
  expect_match(buildNoLanguageModelAvailableMessage(), "deterministic equations", fixed = TRUE)
})

test_that("fit model validation messages preserve formula and response wording", {
  expect_identical(
    buildTooManyPredictorsMessage(c("x1", "x2", "x3", "x4")),
    "This app only allows models with at most 3 covariates. Your formula currently uses 4 predictors: x1, x2, x3, x4."
  )

  expect_identical(
    buildLinearModelBinaryFactorRecodingMessage("passed", c("no", "yes")),
    "The response variable 'passed' is a 2-level factor.\nFor linear regression, it has been recoded to numeric.\nCoding used:  no -> 0,   yes -> 1"
  )
})

test_that("logistic response messages preserve validation wording", {
  expect_identical(
    buildLogisticCharacterResponseMessage("result", 3),
    "Logistic regression requires a binary response. result is a character vector with 3 distinct values."
  )

  expect_identical(
    buildLogisticFactorResponseMessage("group", 4),
    "Logistic regression requires a factor with 2 levels. group has 4 levels."
  )

  expect_identical(
    buildLogisticNumericResponseMessage("y", c(0, 1, 2, 5, 8, 13)),
    "Numeric logistic responses must be 0/1. y has values: 0, 1, 2, 5, 8 ..."
  )

  expect_identical(
    buildLogisticUnsupportedResponseMessage(),
    "Logistic regression requires either a binary factor, numeric 0/1, or a 2-level character vector."
  )
})

test_that("poisson and unknown model messages preserve wording", {
  expect_identical(
    buildPoissonResponseWarningMessage(),
    "Warning: response has negative or non-integer values. Poisson regression expects non-negative counts."
  )

  expect_identical(buildUnknownModelTypeMessage(), "Unknown model type.")
})
