test_that("grade.wmfmModel guards large LLM grading jobs", {
  df = data.frame(
    y = c(1, 2, 3, 4, 5),
    x = c(0, 1, 2, 3, 4)
  )

  model = stats::lm(y ~ x, data = df)

  wm = newWmfmModel(
    model = model,
    formula = y ~ x,
    modelType = "lm",
    data = df,
    equations = "y = a + bx",
    explanation = "Each one-unit increase in x is associated with an average increase in y."
  )

  explanations = rep("Higher x values are associated with higher y values.", 5)

  expect_error(
    grade(
      wm,
      explanation = explanations,
      method = "llm",
      nLlm = 5,
      autoScore = FALSE,
      maxLlmJobsWithoutConfirmation = 20
    ),
    "Set `confirmLargeLlmJob = TRUE` to proceed"
  )
})
