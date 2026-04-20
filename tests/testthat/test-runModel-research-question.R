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
