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
