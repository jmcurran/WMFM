testthat::test_that("research question guidance helper feeds both prompt paths", {
  languageGuidance = getResearchQuestionGuidanceLines(context = "languageContract")
  explanationGuidance = getResearchQuestionGuidanceLines(context = "explanationBlock")

  testthat::expect_true(length(languageGuidance) > 0)
  testthat::expect_true(length(explanationGuidance) > 0)

  languageContract = buildWmfmLanguageContractText(context = "summary")
  testthat::expect_match(languageContract, languageGuidance[[1]], fixed = TRUE)

  df = data.frame(
    Exam = c(42, 58, 81, 86, 35, 72, 42, 25, 36, 48),
    Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7, 7.3, 10.9, 10.9, 9.1)
  )

  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_research_question") = "Does Exam tend to increase as Test increases?"

  prompt = lmToExplanationPrompt(model)

  for (line in explanationGuidance) {
    testthat::expect_match(prompt, line, fixed = TRUE)
  }
})
