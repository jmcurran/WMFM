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
  testthat::expect_match(
    prompt,
    "Start with a short opening paragraph that briefly restates the research question in clear, natural language.",
    fixed = TRUE
  )
  testthat::expect_match(
    prompt,
    "End with a short final paragraph that directly answers the research question in plain language.",
    fixed = TRUE
  )
  testthat::expect_match(
    prompt,
    "Start the final paragraph with a clear answer cue",
    fixed = TRUE
  )
  testthat::expect_match(
    prompt,
    "Do not end with a generic statement",
    fixed = TRUE
  )
})

testthat::test_that("lmToExplanationPrompt omits research question block when absent", {
  df = data.frame(
    Exam = c(42, 58, 81, 86, 35, 72, 42, 25, 36, 48),
    Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7, 7.3, 10.9, 10.9, 9.1)
  )

  model = stats::lm(Exam ~ Test, data = df)

  prompt = lmToExplanationPrompt(model)

  testthat::expect_no_match(prompt, "Research question supplied by the user", fixed = TRUE)
  testthat::expect_no_match(
    prompt,
    "Start with a short opening paragraph that briefly restates the research question in clear, natural language.",
    fixed = TRUE
  )
  testthat::expect_no_match(
    prompt,
    "End with a short final paragraph that directly answers the research question in plain language.",
    fixed = TRUE
  )
  testthat::expect_no_match(
    prompt,
    "Start the final paragraph with a clear answer cue",
    fixed = TRUE
  )
})


test_that("lmToExplanationPrompt includes cautious CI and non-redundant final answer guidance", {
  df = getStats20xExamTestData()[, c("Exam", "Test")]
  fit = stats::lm(Exam ~ Test, data = df)
  attr(fit, "wmfm_research_question") = "How does final exam mark tend to change as the test mark changes?"
  attr(fit, "wmfm_dataset_doc") = "Stats 20x Summer School Data"
  attr(fit, "wmfm_dataset_name") = "s20x"

  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "Approximate proportion of variation explained by the model: 59%", fixed = TRUE)
  testthat::expect_match(prompt, "Do not infer course level", fixed = TRUE)
  testthat::expect_match(prompt, "Do not repeat model-fit statistics in the final paragraph", fixed = TRUE)
  testthat::expect_match(prompt, "Do not write that the true effect is likely to fall inside the confidence interval", fixed = TRUE)
})


testthat::test_that("lmToExplanationPrompt gives inferential framing for intercept-only models", {
  df = getStats20xExamTestData()[, "Exam", drop = FALSE]
  fit = stats::lm(Exam ~ 1, data = df)
  attr(fit, "wmfm_research_question") = "What is the average final exam mark in the course data?"

  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_no_match(prompt, "Approximate proportion of variation explained", fixed = TRUE)
  testthat::expect_match(prompt, "underlying average", fixed = TRUE)
  testthat::expect_match(prompt, "do not describe it as only the average in this data set or sample", fixed = TRUE)
  testthat::expect_match(prompt, "maximum of two short paragraphs", fixed = TRUE)
  testthat::expect_match(prompt, "the sentence containing the estimate and confidence interval is the final answer", fixed = TRUE)
  testthat::expect_match(prompt, "one concise answer with the estimate and confidence interval is enough", fixed = TRUE)
  testthat::expect_match(prompt, "the range shows", fixed = TRUE)
  testthat::expect_match(prompt, "this gives a reasonable sense", fixed = TRUE)
  testthat::expect_match(prompt, "For intercept-only models", fixed = TRUE)
})
