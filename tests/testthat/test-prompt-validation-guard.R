testthat::test_that("lmToExplanationPrompt includes validation guard targets", {
  dat = data.frame(
    y = c(1, 3, 4, 6, 8, 9),
    x = c(0, 1, 2, 3, 4, 5)
  )

  fit = stats::lm(y ~ x, data = dat)
  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "Prompt validation guard:", fixed = TRUE)
  testthat::expect_match(prompt, "technicalScaleLeakage", fixed = TRUE)
  testthat::expect_match(prompt, "rawCoefficientShown", fixed = TRUE)
  testthat::expect_match(prompt, "effectWithoutChangeLanguage", fixed = TRUE)
  testthat::expect_match(prompt, "missingAnswer", fixed = TRUE)
  testthat::expect_match(prompt, "missingUncertainty", fixed = TRUE)
  testthat::expect_match(prompt, "do not regenerate automatically", fixed = TRUE)
})

testthat::test_that("logistic prompt validation guard includes odds formatting target", {
  dat = data.frame(
    Pass = factor(c("No", "No", "Yes", "No", "Yes", "Yes", "No", "Yes")),
    Assign = c(4, 7, 8, 10, 11, 13, 15, 18)
  )

  fit = stats::glm(Pass ~ Assign, family = stats::binomial(), data = dat)
  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "oddsShownAsDecimal", fixed = TRUE)
})

testthat::test_that("interaction prompt validation guard includes interaction comparison target", {
  dat = data.frame(
    group = factor(rep(c("A", "B"), each = 6)),
    x = rep(seq_len(6), 2),
    y = c(1, 2, 3, 4, 5, 6, 1, 1, 2, 2, 3, 3)
  )

  fit = stats::lm(y ~ group * x, data = dat)
  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  testthat::expect_match(prompt, "interactionNotCompared", fixed = TRUE)
  testthat::expect_match(prompt, "compare the within-group effects", fixed = TRUE)
})

testthat::test_that("buildModelExplanationAudit records validation guard prompt input", {
  dat = data.frame(
    y = c(1, 3, 4, 6, 8, 9),
    x = c(0, 1, 2, 3, 4, 5)
  )

  fit = stats::lm(y ~ x, data = dat)
  audit = buildModelExplanationAudit(fit)

  testthat::expect_true(audit$promptInputs$promptValidationGuardIncluded)
  testthat::expect_match(
    audit$rawPromptIngredients$promptValidationGuardPrompt,
    "Prompt validation guard:",
    fixed = TRUE
  )
})
