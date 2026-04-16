test_that("buildAppModelOutputs falls back to deterministic equations without chat", {

  df = data.frame(
    y = c(1, 2, 3, 4, 5),
    x = c(0, 1, 2, 3, 4)
  )

  model = lm(y ~ x, data = df)

  out = buildAppModelOutputs(
    model = model,
    chatProvider = NULL
  )

  expect_s3_class(out$equations, "wmfmEquationTable")
  expect_null(out$explanation)
  expect_identical(out$equationMethodUsed, "deterministic")
})

test_that("buildAppModelOutputs uses chat outputs when available", {

  df = data.frame(
    y = c(1, 2, 3, 4, 5),
    x = c(0, 1, 2, 3, 4)
  )

  model = lm(y ~ x, data = df)

  local_mocked_bindings(
    getModelEquations = function(model, method = c("llm", "deterministic"), chat = NULL, digits = 2) {
      method = match.arg(method)

      expect_identical(method, "llm")
      expect_identical(chat, "fake-chat")
      "equation text"
    },
    lmExplanation = function(model, chat, useCache = TRUE) {
      expect_identical(chat, "fake-chat")
      expect_false(useCache)
      "explanation text"
    }
  )

  out = buildAppModelOutputs(
    model = model,
    chatProvider = "fake-chat",
    useExplanationCache = FALSE
  )

  expect_identical(out$equations, "equation text")
  expect_identical(out$explanation, "explanation text")
  expect_identical(out$equationMethodUsed, "llm")
})

test_that("buildAppModelOutputs falls back to deterministic equations if llm equations fail", {

  df = data.frame(
    y = c(1, 2, 3, 4, 5),
    x = c(0, 1, 2, 3, 4)
  )

  model = lm(y ~ x, data = df)

  local_mocked_bindings(
    getModelEquations = function(model, method = c("llm", "deterministic"), chat = NULL, digits = 2) {
      method = match.arg(method)

      if (identical(method, "llm")) {
        stop("equation request failed")
      }

      data.frame(
        condition = "General",
        equation = "y = 1.00 + 1.00 * x",
        linearPredictor = "y = 1.00 + 1.00 * x",
        oddsScale = NA_character_,
        responseScale = "y = 1.00 + 1.00 * x",
        stringsAsFactors = FALSE
      ) |>
        structure(class = c("wmfmEquationTable", "data.frame"))
    },
    lmExplanation = function(model, chat, useCache = TRUE) {
      expect_identical(chat, "fake-chat")
      expect_false(useCache)
      "explanation text"
    }
  )

  out = buildAppModelOutputs(
    model = model,
    chatProvider = "fake-chat",
    useExplanationCache = FALSE
  )

  expect_s3_class(out$equations, "wmfmEquationTable")
  expect_identical(out$explanation, "explanation text")
  expect_identical(out$equationMethodUsed, "deterministic")
})
