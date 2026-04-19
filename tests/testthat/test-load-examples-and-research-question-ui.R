testthat::test_that("renderExplanationTutorUi can show research question and data description", {
  ui = renderExplanationTutorUi(
    text = "The app starts with the question and then explains the fitted result.",
    available = TRUE,
    researchQuestion = "Does Test help explain Exam?",
    dataDescription = "The response variable is `Exam`. Number-valued predictors: `Test`."
  )

  html = as.character(ui)

  testthat::expect_match(html, "Research question:", fixed = TRUE)
  testthat::expect_match(html, "Does Test help explain Exam\\?", perl = TRUE)
  testthat::expect_match(html, "The response variable is", fixed = TRUE)
})
