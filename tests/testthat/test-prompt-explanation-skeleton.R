testthat::test_that("lmToExplanationPrompt includes numeric model-aware skeleton guidance", {
  dat = data.frame(
    y = c(1, 3, 4, 6, 8, 9),
    x = c(0, 1, 2, 3, 4, 5)
  )

  fit = stats::lm(y ~ x, data = dat)
  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "Deterministic explanation skeleton:", fixed = TRUE)
  testthat::expect_match(prompt, "Skeleton id: lm_numericMainEffect", fixed = TRUE)
  testthat::expect_match(prompt, "typicalCase", fixed = TRUE)
  testthat::expect_match(prompt, "effect of a change in the numeric predictor", fixed = TRUE)
  testthat::expect_match(prompt, "clear closing takeaway", fixed = TRUE)
  testthat::expect_match(prompt, "Scale guidance: Explain estimates and effects on the response scale.", fixed = TRUE)
})

testthat::test_that("lmToExplanationPrompt includes within-group-then-compare interaction skeleton", {
  dat = data.frame(
    Locn = factor(c(rep("SC", 9), rep("WA", 9))),
    Magnitude = c(
      5.25, 5.5, 5.75, 6, 6.25, 6.5, 6.75, 7, 7.25,
      5.25, 5.5, 5.75, 6, 6.25, 6.5, 6.75, 7, 7.25
    ),
    Freq = c(32, 27, 10, 9, 6, 6, 3, 2, 2, 13, 6, 2, 1, 0, 0, 1, 0, 0)
  )

  fit = stats::glm(Freq ~ Locn * Magnitude, family = "poisson", data = dat)
  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "Skeleton id: poisson_interaction", fixed = TRUE)
  testthat::expect_match(prompt, "firstWithinGroupEffect", fixed = TRUE)
  testthat::expect_match(prompt, "secondWithinGroupEffect", fixed = TRUE)
  testthat::expect_match(prompt, "effectComparison", fixed = TRUE)
  testthat::expect_match(prompt, "Interaction explanation control:", fixed = TRUE)
  testthat::expect_match(prompt, "Use a within-group-then-compare structure", fixed = TRUE)
  testthat::expect_match(prompt, "compare those within-group effects directly", fixed = TRUE)
  testthat::expect_match(prompt, "Do not explain by adding, subtracting, or decomposing model coefficients", fixed = TRUE)
  testthat::expect_match(prompt, "Do not use the phrase interaction term", fixed = TRUE)
  testthat::expect_no_match(prompt, "component-by-component", fixed = TRUE)
})

testthat::test_that("buildModelExplanationAudit records skeleton prompt input", {
  dat = data.frame(
    y = c(1, 3, 4, 6, 8, 9),
    x = c(0, 1, 2, 3, 4, 5)
  )

  fit = stats::lm(y ~ x, data = dat)
  audit = buildModelExplanationAudit(fit)

  testthat::expect_true(audit$promptInputs$explanationSkeletonIncluded)
  testthat::expect_match(
    audit$rawPromptIngredients$explanationSkeletonPrompt,
    "Deterministic explanation skeleton:",
    fixed = TRUE
  )
})

testthat::test_that("lmToExplanationPrompt gives concise intercept-only answer guidance after 12.2.8", {
  model = stats::lm(Exam ~ 1, data = data.frame(Exam = c(54, 66, 78, 102)))
  prompt = lmToExplanationPrompt(model)

  testthat::expect_match(
    prompt,
    "Restate the research question inferentially",
    fixed = TRUE
  )
  testthat::expect_match(
    prompt,
    "this sentence is both the estimate, the uncertainty statement, and the final answer",
    fixed = TRUE
  )
  testthat::expect_match(
    prompt,
    "Do not add a separate range explanation",
    fixed = TRUE
  )
  testthat::expect_match(
    prompt,
    "We estimate this value",
    fixed = TRUE
  )
  testthat::expect_match(
    prompt,
    "we can be 95% confident",
    fixed = TRUE
  )
})


testthat::test_that("lmToExplanationPrompt includes Stage 13.2 wording controls", {
  dat = data.frame(
    Exam = c(42, 48, 55, 60, 65, 72, 77, 80),
    Attend = factor(c("No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes")),
    Test = c(8, 9, 11, 12, 10, 12, 13, 15)
  )

  fit = stats::lm(Exam ~ Attend + Test, data = dat)
  attr(fit, "wmfm_research_question") = paste(
    "How do attendance and test mark relate to final exam mark,",
    "without allowing the test-mark slope to differ by attendance?"
  )
  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(
    prompt,
    "You may say the model when describing results",
    fixed = TRUE
  )
  testthat::expect_match(
    prompt,
    "do not use qualified labels such as fitted model, linear model, logistic model, or Poisson model",
    fixed = TRUE
  )
  testthat::expect_match(
    prompt,
    "do not replace mark with score",
    fixed = TRUE
  )
  testthat::expect_match(
    prompt,
    "When reporting group fitted values from a model that also has numeric predictors",
    fixed = TRUE
  )
  testthat::expect_match(
    prompt,
    "for a student with an average test mark",
    fixed = TRUE
  )
  testthat::expect_match(
    prompt,
    "avoid abstract phrases such as per unit increase",
    fixed = TRUE
  )
  testthat::expect_match(
    prompt,
    "Do not use overlap or non-overlap of separate confidence intervals",
    fixed = TRUE
  )
})


testthat::test_that("interaction skeleton asks for numeric support for effect comparisons", {
  dat = data.frame(
    Exam = c(42, 48, 55, 60, 65, 72, 77, 80),
    Attend = factor(c("No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes")),
    Test = c(8, 9, 11, 12, 10, 12, 13, 15)
  )

  fit = stats::lm(Exam ~ Attend * Test, data = dat)
  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "Skeleton id: lm_interaction", fixed = TRUE)
  testthat::expect_match(
    prompt,
    "using numeric estimates where available rather than relying only on words",
    fixed = TRUE
  )
  testthat::expect_match(
    prompt,
    "include quantitative estimates for the within-group effects or the difference between those effects",
    fixed = TRUE
  )
})
