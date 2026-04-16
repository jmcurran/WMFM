test_that("getModelEquations returns deterministic equation tables by default", {
  df = data.frame(
    y = c(1, 3, 5, 7),
    x = c(0, 1, 2, 3)
  )

  model = stats::lm(y ~ x, data = df)
  out = getModelEquations(model)

  expect_s3_class(out, "data.frame")
  expect_s3_class(out, "wmfmEquationTable")
  expect_true(all(c(
    "condition",
    "equation",
    "linearPredictor",
    "oddsScale",
    "responseScale"
  ) %in% names(out)))
  expect_equal(nrow(out), 1)
  expect_match(out$equation[[1]], "y =")
})

test_that("getModelEquations requires chat for llm equations", {
  df = data.frame(
    y = c(1, 3, 5, 7),
    x = c(0, 1, 2, 3)
  )

  model = stats::lm(y ~ x, data = df)

  expect_warning(
    expect_error(
      getModelEquations(model, method = "llm"),
      "`chat` must be supplied"
    ),
    "deprecated"
  )
})

test_that("runModel attaches deterministic equations by default without chat access", {
  testthat::local_mocked_bindings(
    getChatProvider = function() {
      stop("chat unavailable", call. = FALSE)
    },
    .package = "WMFM"
  )

  df = data.frame(
    y = c(1, 3, 5, 7),
    x = c(0, 1, 2, 3)
  )

  out = NULL

  expect_warning(
    out <- runModel(
      data = df,
      formula = y ~ x,
      modelType = "lm",
      printOutput = FALSE
    ),
    "Could not connect to the language model server"
  )

  expect_s3_class(out, "wmfmModel")
  expect_s3_class(out$equations, "wmfmEquationTable")
  expect_true(is.null(out$explanation))
  expect_identical(out$meta$equationMethod, "deterministic")
  expect_identical(out$meta$equationMethodUsed, "deterministic")
})

test_that("runModel falls back to deterministic equations when llm equations fail", {
  testthat::local_mocked_bindings(
    getChatProvider = function() {
      "fake-chat"
    },
    getModelEquations = function(model, method = c("deterministic", "llm"), chat = NULL, digits = 2) {
      method = match.arg(method)

      if (identical(method, "llm")) {
        stop("llm equations failed", call. = FALSE)
      }

      buildDeterministicEquationTable(model)
    },
    lmExplanation = function(model, chat, useCache = TRUE) {
      "explanation text"
    },
    .package = "WMFM"
  )

  df = data.frame(
    y = c(1, 3, 5, 7),
    x = c(0, 1, 2, 3)
  )

  out = NULL

  expect_warning(
    out <- runModel(
      data = df,
      formula = y ~ x,
      modelType = "lm",
      printOutput = FALSE,
      equationMethod = "llm"
    ),
    "Falling back to deterministic equations"
  )

  expect_s3_class(out, "wmfmModel")
  expect_s3_class(out$equations, "wmfmEquationTable")
  expect_identical(out$explanation, "explanation text")
  expect_identical(out$meta$equationMethod, "llm")
  expect_identical(out$meta$equationMethodUsed, "deterministic")
})

test_that("deterministic equation tables are stable for representative glm models", {
  logisticDf = data.frame(
    pass = factor(c("Fail", "Fail", "Pass", "Pass", "Pass", "Fail")),
    x = c(-2, -1, 0, 1, 2, 0.5)
  )
  logisticModel = stats::glm(
    pass ~ x,
    data = logisticDf,
    family = stats::binomial(link = "logit")
  )

  logisticOut = getModelEquations(logisticModel)
  expect_s3_class(logisticOut, "wmfmEquationTable")
  expect_match(logisticOut$linearPredictor[[1]], "logit")
  expect_match(logisticOut$oddsScale[[1]], "Odds")
  expect_match(logisticOut$responseScale[[1]], "Pr")

  poissonDf = data.frame(
    y = c(1, 2, 2, 3, 4, 5),
    x = c(0, 1, 2, 3, 4, 5)
  )
  poissonModel = stats::glm(
    y ~ x,
    data = poissonDf,
    family = stats::poisson(link = "log")
  )

  poissonOut = getModelEquations(poissonModel)
  expect_s3_class(poissonOut, "wmfmEquationTable")
  expect_match(poissonOut$linearPredictor[[1]], "log")
  expect_match(poissonOut$responseScale[[1]], "exp")
})
