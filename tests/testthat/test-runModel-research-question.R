testthat::test_that("runModel attaches a research question to the fitted model and wmfmModel output", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]

  testthat::local_mocked_bindings(
    getModelEquations = function(model, method = "deterministic", chat = NULL) {
      "Exam = a + b * Test"
    },
    getChatProvider = function(...) {
      NULL
    },
    .package = "WMFM"
  )

  out = runModel(
    data = df,
    formula = Exam ~ Test,
    modelType = "lm",
    researchQuestion = "Does Test help explain Exam?",
    printOutput = FALSE
  )

  testthat::expect_s3_class(out, "wmfmModel")
  testthat::expect_identical(out$researchQuestion, "Does Test help explain Exam?")
  testthat::expect_identical(
    attr(out$model, "wmfm_research_question", exact = TRUE),
    "Does Test help explain Exam?"
  )
})

testthat::test_that("runModel allows four covariates for adjusted log-log examples", {
  df = expand.grid(
    x1 = seq(1, 6),
    x2 = c("a", "b"),
    x3 = c("c", "d"),
    x4 = c("e", "f")
  )
  df$y = 10 +
    2 * df$x1 +
    ifelse(df$x2 == "b", 1.5, 0) +
    ifelse(df$x3 == "d", -0.75, 0) +
    ifelse(df$x4 == "f", 0.5, 0)

  testthat::local_mocked_bindings(
    getModelEquations = function(model, method = "deterministic", chat = NULL) {
      "y = a + b1 * x1 + b2 * x2 + b3 * x3 + b4 * x4"
    },
    getChatProvider = function(...) {
      NULL
    },
    .package = "WMFM"
  )

  out = runModel(
    data = df,
    formula = y ~ x1 + x2 + x3 + x4,
    modelType = "lm",
    printOutput = FALSE
  )

  testthat::expect_s3_class(out, "wmfmModel")
})
