testthat::test_that("lmToExplanationPrompt includes research question guidance when present", {
  df = data.frame(
    Exam = c(42, 58, 81, 86, 35, 72, 42, 25, 36, 48),
    Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7, 7.3, 10.9, 10.9, 9.1)
  )

  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_research_question") = "Does Exam tend to increase as Test increases?"

  prompt = lmToExplanationPrompt(model)

  testthat::expect_match(prompt, "Research question supplied by the user", fixed = TRUE)
  testthat::expect_match(
    prompt,
    "Does Exam tend to increase as Test increases\\?",
    perl = TRUE
  )
  testthat::expect_match(prompt, "answer the research question", fixed = TRUE)
})

testthat::test_that("lmToExplanationPrompt omits research question block when absent", {
  df = data.frame(
    Exam = c(42, 58, 81, 86, 35, 72, 42, 25, 36, 48),
    Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7, 7.3, 10.9, 10.9, 9.1)
  )

  model = stats::lm(Exam ~ Test, data = df)

  prompt = lmToExplanationPrompt(model)

  testthat::expect_no_match(prompt, "Research question supplied by the user", fixed = TRUE)
  testthat::expect_no_match(prompt, "answer the research question", fixed = TRUE)
})
