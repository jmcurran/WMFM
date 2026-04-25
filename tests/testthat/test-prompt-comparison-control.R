testthat::test_that("lmToExplanationPrompt limits exhaustive factor comparisons", {
  dat = data.frame(
    y = c(10, 11, 12, 12, 20, 21, 19, 22, 31, 29, 30, 32, 40, 42, 39, 41),
    treatment = factor(rep(c("A", "B", "C", "D"), each = 4))
  )

  fit = stats::lm(y ~ treatment, data = dat)
  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "Comparison control:", fixed = TRUE)
  testthat::expect_match(prompt, "Comparison scope: minimal", fixed = TRUE)
  testthat::expect_match(prompt, "Do not enumerate all treatment, group, or factor-level comparisons", fixed = TRUE)
  testthat::expect_match(prompt, "treatment: 4 levels; 6 possible pairwise comparisons", fixed = TRUE)
  testthat::expect_match(prompt, "summarise the factor effect rather than listing pairwise contrasts", fixed = TRUE)
})

testthat::test_that("lmToExplanationPrompt allows targeted comparison guidance when a research question is supplied", {
  dat = data.frame(
    y = c(10, 11, 12, 12, 20, 21, 19, 22, 31, 29, 30, 32),
    treatment = factor(rep(c("control", "low", "high"), each = 4))
  )

  fit = stats::lm(y ~ treatment, data = dat)
  attr(fit, "wmfm_research_question") = "Is the high treatment different from the control treatment?"
  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "Comparison control:", fixed = TRUE)
  testthat::expect_match(prompt, "Research question supplied: yes", fixed = TRUE)
  testthat::expect_match(prompt, "Use targeted comparisons only when they are relevant", fixed = TRUE)
  testthat::expect_no_match(prompt, "With no research question asking for specific pairs", fixed = TRUE)
})

testthat::test_that("buildModelExplanationAudit records comparison-control prompt input", {
  dat = data.frame(
    y = c(10, 11, 12, 12, 20, 21, 19, 22, 31, 29, 30, 32),
    treatment = factor(rep(c("control", "low", "high"), each = 4))
  )

  fit = stats::lm(y ~ treatment, data = dat)
  audit = buildModelExplanationAudit(fit)

  testthat::expect_true(audit$promptInputs$comparisonControlIncluded)
  testthat::expect_match(
    audit$rawPromptIngredients$comparisonControlPrompt,
    "Comparison control:",
    fixed = TRUE
  )
})
