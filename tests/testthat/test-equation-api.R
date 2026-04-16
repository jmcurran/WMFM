test_that("getModelEquations returns deterministic equation tables", {
  df = data.frame(
    y = c(1, 3, 5, 7),
    x = c(0, 1, 2, 3)
  )

  model = stats::lm(y ~ x, data = df)
  out = getModelEquations(model, method = "deterministic")

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

  expect_error(
    getModelEquations(model, method = "llm"),
    "`chat` must be supplied"
  )
})

test_that("runModel can attach deterministic equations without chat access", {
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
      printOutput = FALSE,
      equationMethod = "deterministic"
    ),
    "Could not connect to the language model server"
  )

  expect_s3_class(out, "wmfmModel")
  expect_s3_class(out$equations, "wmfmEquationTable")
  expect_true(is.null(out$explanation))
  expect_identical(out$meta$equationMethod, "deterministic")
})
