test_that("buildModelNumericAnchorInfo reports mean anchor when zero is outside the data range", {
  dat = data.frame(
    y = c(1, 2, 3, 4),
    x = c(5, 6, 7, 8)
  )

  fit = stats::lm(y ~ x, data = dat)
  info = buildModelNumericAnchorInfo(fit)

  expect_identical(info$numericReference, "mean")
  expect_match(info$promptText, "x: observed range = [5, 8]", fixed = TRUE)
  expect_match(info$promptText, "chosen anchor = 6.5", fixed = TRUE)
  expect_match(info$promptText, "0 lies outside the observed range", fixed = TRUE)
  expect_match(info$cacheKey, "mean", fixed = TRUE)
  expect_match(info$cacheKey, "x=5:8=6.5", fixed = TRUE)
})

test_that("lmToExplanationPrompt includes numeric anchor guidance", {
  dat = data.frame(
    y = c(2, 3, 5, 7),
    Magnitude = c(5.25, 5.5, 6, 7.25),
    Locn = factor(c("SC", "SC", "WA", "WA"))
  )

  fit = stats::glm(y ~ Locn * Magnitude, family = "poisson", data = dat)
  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  expect_match(prompt, "Numeric interpretation anchor:", fixed = TRUE)
  expect_match(prompt, "Magnitude: observed range = [5.25, 7.25]", fixed = TRUE)
  expect_match(prompt, "do not describe the intercept as if it were directly meaningful at 0", fixed = TRUE)
})

test_that("lmToPrompt includes numeric anchor guidance for equations", {
  dat = data.frame(
    y = c(2, 3, 5, 7),
    Magnitude = c(5.25, 5.5, 6, 7.25),
    Locn = factor(c("SC", "SC", "WA", "WA"))
  )

  fit = stats::glm(y ~ Locn * Magnitude, family = "poisson", data = dat)
  prompt = suppressWarnings(lmToPrompt(fit))

  expect_match(prompt, "Numeric interpretation anchor:", fixed = TRUE)
  expect_match(prompt, "Formal fitted equations may still be written as functions of the numeric predictor itself.", fixed = TRUE)
  expect_match(prompt, "Do not describe an intercept or baseline fitted value as being 'when X = 0' unless 0 lies inside the observed range", fixed = TRUE)
})

test_that("lmToExplanationPrompt handles logistic models when zero lies inside the data range", {
  dat = data.frame(
    y = c(0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0),
    x = c(-2.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5)
  )

  fit = suppressWarnings(stats::glm(y ~ x, family = "binomial", data = dat))
  prompt = suppressWarnings(lmToExplanationPrompt(fit))

  expect_match(prompt, "This is a generalised linear model with binomial family and logit link.", fixed = TRUE)
  expect_match(prompt, "x: observed range = [-2, 3.5]; chosen anchor = 0 (0 lies inside the observed range.)", fixed = TRUE)
  expect_match(prompt, "Keep the inferential register cautious and evidence-based.", fixed = TRUE)
  expect_match(prompt, 'Do not say that the data "confirm", "prove", "establish", or "demonstrate" an effect.', fixed = TRUE)
})
