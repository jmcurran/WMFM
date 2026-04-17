testthat::test_that("runModel attaches a research question to the fitted model and wmfmModel output", {
  df = data.frame(
    y = c(1, 2, 3, 4, 5),
    x = c(0, 1, 2, 3, 4)
  )

  testthat::local_mocked_bindings(
    getModelEquations = function(model, method = "deterministic", chat = NULL) {
      "y = 1.00 + 1.00 * x"
    },
    getChatProvider = function(...) {
      NULL
    },
    .package = "WMFM"
  )

  out = runModel(
    data = df,
    formula = y ~ x,
    modelType = "lm",
    researchQuestion = "Does x help explain y?",
    printOutput = FALSE
  )

  testthat::expect_s3_class(out, "wmfmModel")
  testthat::expect_identical(out$researchQuestion, "Does x help explain y?")
  testthat::expect_identical(
    attr(out$model, "wmfm_research_question", exact = TRUE),
    "Does x help explain y?"
  )
})
